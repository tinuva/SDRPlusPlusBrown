#include <imgui.h>
#include <module.h>
#include <gui/gui.h>
#include <signal_path/signal_path.h>
#include <signal_path/sink.h>
#include <dsp/buffer/packer.h>
#include <dsp/convert/stereo_to_mono.h>
#include <utils/flog.h>
#include <config.h>
#include <core.h>
#include <AudioToolbox/AudioToolbox.h>

#define CONCAT(a, b) ((std::string(a) + b).c_str())

SDRPP_MOD_INFO{
    /* Name:            */ "macos_coreaudio_sink",
    /* Description:     */ "CoreAudio sink module for macOS",
    /* Author:          */ "Sanny Sanoff and his aider",
    /* Version:         */ 0, 1, 0,
    /* Max instances    */ 1
};

ConfigManager config;

class CoreAudioSink : SinkManager::Sink {
public:
    struct AudioDevice {
        AudioDeviceID id;
        std::string name;
        std::vector<double> sampleRates;
        std::string sampleRatesTxt;
    };

    CoreAudioSink(SinkManager::Stream* stream, std::string streamName) {
        _stream = stream;
        _streamName = streamName;
        s2m.init(_stream->sinkOut);
        stereoPacker.init(_stream->sinkOut, 8192);

        // Get available devices
        enumerateDevices();

        // Load config
        config.acquire();
        if (!config.conf.contains(_streamName)) {
            config.conf[_streamName]["device"] = "";
            config.conf[_streamName]["devices"] = json({});
        }
        std::string device = config.conf[_streamName]["device"];
        config.release(true);

        selectByName(device);
    }

    ~CoreAudioSink() {
        stop();
    }

    void start() {
        if (running) { return; }
        running = doStart();
    }

    void stop() {
        if (!running) { return; }
        doStop();
        running = false;
    }

    void selectFirst() {
        if (!devices.empty()) {
            selectById(0);
        }
    }

    void selectByName(std::string name) {
        for (int i = 0; i < devices.size(); i++) {
            if (devices[i].name == name) {
                selectById(i);
                return;
            }
        }
        selectFirst();
    }

    void selectById(int id) {
        if (id < 0 || id >= devices.size()) { return; }
        
        devId = id;
        auto& dev = devices[devId];

        // Load sample rate from config
        config.acquire();
        if (!config.conf[_streamName]["devices"].contains(dev.name)) {
            config.conf[_streamName]["devices"][dev.name] = dev.sampleRates[0];
        }
        sampleRate = config.conf[_streamName]["devices"][dev.name];
        config.release(true);

        // Find sample rate ID
        bool found = false;
        for (int i = 0; i < dev.sampleRates.size(); i++) {
            if (dev.sampleRates[i] == sampleRate) {
                srId = i;
                found = true;
                break;
            }
        }
        if (!found) {
            sampleRate = dev.sampleRates[0];
            srId = 0;
        }

        _stream->setSampleRate(sampleRate);

        // Update audio unit with new device
        if (audioUnit) {
            AudioDeviceID newDeviceID = devices[devId].id;
            OSStatus status = AudioUnitSetProperty(audioUnit,
                                                 kAudioOutputUnitProperty_CurrentDevice,
                                                 kAudioUnitScope_Global,
                                                 0,
                                                 &newDeviceID,
                                                 sizeof(newDeviceID));
            if (status != noErr) {
                flog::error("Failed to set audio unit device");
            }
        }
        
        // Restart audio if running
        if (running) {
            doStop();
            doStart();
        }
    }

    void menuHandler() {
        float menuWidth = ImGui::GetContentRegionAvail().x;

        // Device selection
        ImGui::SetNextItemWidth(menuWidth);
        if (ImGui::Combo(("##_coreaudio_sink_dev_" + _streamName).c_str(), &devId, [](void* data, int idx, const char** out_text) {
            auto devices = (std::vector<AudioDevice>*)data;
            *out_text = devices->at(idx).name.c_str();
            return true;
        }, &devices, devices.size())) {
            selectById(devId);
            config.acquire();
            config.conf[_streamName]["device"] = devices[devId].name;
            config.release(true);
        }

        // Sample rate selection
        if (devId >= 0 && devId < devices.size()) {
            ImGui::SetNextItemWidth(menuWidth);
            if (ImGui::Combo(("##_coreaudio_sink_sr_" + _streamName).c_str(), &srId, devices[devId].sampleRatesTxt.c_str())) {
                sampleRate = devices[devId].sampleRates[srId];
                _stream->setSampleRate(sampleRate);
                if (running) {
                    doStop();
                    doStart();
                }
                config.acquire();
                config.conf[_streamName]["devices"][devices[devId].name] = sampleRate;
                config.release(true);
            }
        }
        
        if (underflow != 0) {
            // ImGui::SameLine();
            // ImGui::Text("Underflow %d", underflow);
        }
    }

private:
    bool doStart() {
        if (devId < 0 || devId >= devices.size()) { return false; }
        
        auto& dev = devices[devId];
        
        // Create output audio unit for the selected device
        AudioComponentDescription desc = {
            .componentType = kAudioUnitType_Output,
            .componentSubType = kAudioUnitSubType_HALOutput,  // Use HAL for specific device
            .componentManufacturer = kAudioUnitManufacturer_Apple,
            .componentFlags = 0,
            .componentFlagsMask = 0
        };

        AudioComponent comp = AudioComponentFindNext(NULL, &desc);
        if (!comp) {
            flog::error("Could not find audio component for device");
            return false;
        }

        // Set the selected device ID
        OSStatus status = AudioUnitSetProperty(audioUnit,
                                             kAudioOutputUnitProperty_CurrentDevice,
                                             kAudioUnitScope_Global,
                                             0,
                                             &dev.id,
                                             sizeof(dev.id));
        if (status != noErr) {
            flog::error("Could not set audio unit device");
            return false;
        }
        if (!comp) {
            flog::error("Could not find audio component");
            return false;
        }

        OSStatus status = AudioComponentInstanceNew(comp, &audioUnit);
        if (status != noErr) {
            flog::error("Could not create audio unit instance");
            return false;
        }

        // Set stream format with lower latency settings
        AudioStreamBasicDescription streamFormat = {
            .mSampleRate = sampleRate,
            .mFormatID = kAudioFormatLinearPCM,
            .mFormatFlags = kAudioFormatFlagIsFloat | kAudioFormatFlagIsPacked | kAudioFormatFlagIsNonInterleaved,
            .mBytesPerPacket = sizeof(float),
            .mFramesPerPacket = 1,
            .mBytesPerFrame = sizeof(float),
            .mChannelsPerFrame = 2,
            .mBitsPerChannel = sizeof(float) * 8,
            .mReserved = 0
        };

        status = AudioUnitSetProperty(audioUnit,
                                    kAudioUnitProperty_StreamFormat,
                                    kAudioUnitScope_Input,
                                    0,
                                    &streamFormat,
                                    sizeof(streamFormat));
        if (status != noErr) {
            flog::error("Could not set stream format");
            return false;
        }

        // Set render callback
        AURenderCallbackStruct callback = {
            .inputProc = renderCallback,
            .inputProcRefCon = this
        };

        status = AudioUnitSetProperty(audioUnit,
                                    kAudioUnitProperty_SetRenderCallback,
                                    kAudioUnitScope_Input,
                                    0,
                                    &callback,
                                    sizeof(callback));
        if (status != noErr) {
            flog::error("Could not set render callback");
            return false;
        }

        // Initialize audio unit
        status = AudioUnitInitialize(audioUnit);
        if (status != noErr) {
            flog::error("Could not initialize audio unit");
            return false;
        }

        // Set buffer frame size for lower latency
        UInt32 bufferFrameSize = 4096;
        status = AudioUnitSetProperty(audioUnit,
                                    kAudioDevicePropertyBufferFrameSize,
                                    kAudioUnitScope_Global,
                                    0,
                                    &bufferFrameSize,
                                    sizeof(bufferFrameSize));
        if (status != noErr) {
            flog::warn("Could not set buffer frame size, using default");
        }

        // Start audio unit
        status = AudioOutputUnitStart(audioUnit);
        if (status != noErr) {
            flog::error("Could not start audio unit");
            return false;
        }

        // Set packer buffer size to match audio unit buffer size
        stereoPacker.setSampleCount(bufferFrameSize);
        stereoPacker.start();
        return true;
    }

    void doStop() {
        if (audioUnit) {
            AudioOutputUnitStop(audioUnit);
            AudioUnitUninitialize(audioUnit);
            AudioComponentInstanceDispose(audioUnit);
            audioUnit = nullptr;
        }
        stereoPacker.stop();
    }

    std::vector<dsp::stereo_t> stereoBuffer;

    static OSStatus renderCallback(void* inRefCon,
                                 AudioUnitRenderActionFlags* ioActionFlags,
                                 const AudioTimeStamp* inTimeStamp,
                                 UInt32 inBusNumber,
                                 UInt32 inNumberFrames,
                                 AudioBufferList* ioData) {
        CoreAudioSink* _this = (CoreAudioSink*)inRefCon;

        // Get pointers to output buffers
        float* left = (float*)ioData->mBuffers[0].mData;
        float* right = (float*)ioData->mBuffers[1].mData;

        if (!gui::mainWindow.isPlaying()) {
            memset(left, 0, inNumberFrames * sizeof(float));
            memset(right, 0, inNumberFrames * sizeof(float));
            return noErr;
        }


        memset(left, 0, inNumberFrames * sizeof(float));
        memset(right, 0, inNumberFrames * sizeof(float));

        // Read audio data from packer
        int count = 0;
        if (_this->stereoPacker.out.isDataReady()) {
            count = _this->stereoPacker.out.read();
            if (count <= 0) {
                return noErr;
            }
        }
        _this->stereoBuffer.resize(_this->stereoBuffer.size() + count);

        // replace with loop
        for (int i = 0; i < count; i++) {
            _this->stereoBuffer[i] = _this->stereoPacker.out.readBuf[i];
        }

        int limit = std::min<uint32_t>(inNumberFrames, _this->stereoBuffer.size());

        // Copy data to output buffers
        for (UInt32 i = 0; i < limit; i++) {
            left[i] = _this->stereoBuffer[i].l;
            right[i] = _this->stereoBuffer[i].r;
        }
        if (limit < inNumberFrames) {
            _this->underflow = 1;
        } else {
            _this->underflow = 0;
        }

        _this->stereoBuffer.erase(_this->stereoBuffer.begin(), _this->stereoBuffer.begin() + limit);
        _this->stereoPacker.out.flush();
        return noErr;
    }

    void enumerateDevices() {
        AudioObjectPropertyAddress prop = {
            .mSelector = kAudioHardwarePropertyDevices,
            .mScope = kAudioObjectPropertyScopeGlobal,
            .mElement = kAudioObjectPropertyElementMain
        };

        UInt32 size = 0;
        OSStatus status = AudioObjectGetPropertyDataSize(kAudioObjectSystemObject, &prop, 0, NULL, &size);
        if (status != noErr) {
            flog::error("Could not get audio devices size");
            return;
        }

        UInt32 count = size / sizeof(AudioDeviceID);
        AudioDeviceID* deviceIDs = new AudioDeviceID[count];
        status = AudioObjectGetPropertyData(kAudioObjectSystemObject, &prop, 0, NULL, &size, deviceIDs);
        if (status != noErr) {
            flog::error("Could not get audio devices");
            delete[] deviceIDs;
            return;
        }

        for (UInt32 i = 0; i < count; i++) {
            AudioDevice device;
            device.id = deviceIDs[i];

            // Get device name
            prop.mSelector = kAudioObjectPropertyName;
            CFStringRef name = NULL;
            size = sizeof(name);
            status = AudioObjectGetPropertyData(device.id, &prop, 0, NULL, &size, &name);
            if (status == noErr && name) {
                char buffer[256];
                CFStringGetCString(name, buffer, 256, kCFStringEncodingUTF8);
                device.name = buffer;
                CFRelease(name);
            }

            // Get supported sample rates
            prop.mSelector = kAudioDevicePropertyAvailableNominalSampleRates;
            status = AudioObjectGetPropertyDataSize(device.id, &prop, 0, NULL, &size);
            if (status == noErr) {
                AudioValueRange* ranges = (AudioValueRange*)malloc(size);
                status = AudioObjectGetPropertyData(device.id, &prop, 0, NULL, &size, ranges);
                if (status == noErr) {
                    UInt32 rangeCount = size / sizeof(AudioValueRange);
                    for (UInt32 j = 0; j < rangeCount; j++) {
                        double min = ranges[j].mMinimum;
                        double max = ranges[j].mMaximum;
                        // Add common rates between min and max
                        if (min <= 44100 && max >= 44100) device.sampleRates.push_back(44100);
                        if (min <= 48000 && max >= 48000) device.sampleRates.push_back(48000);
                        if (min <= 96000 && max >= 96000) device.sampleRates.push_back(96000);
                        if (min <= 192000 && max >= 192000) device.sampleRates.push_back(192000);
                    }
                    // Sort and create text list
                    std::sort(device.sampleRates.begin(), device.sampleRates.end());
                    for (auto sr : device.sampleRates) {
                        device.sampleRatesTxt += std::to_string((int)sr);
                        device.sampleRatesTxt += '\0';
                    }
                }
                free(ranges);
            }

            if (!device.sampleRates.empty()) {
                devices.push_back(device);
            }
        }

        delete[] deviceIDs;
    }

    SinkManager::Stream* _stream;
    dsp::convert::StereoToMono s2m;
    dsp::buffer::Packer<dsp::stereo_t> stereoPacker;

    std::string _streamName;

    int srId = 0;
    int devId = -1;
    bool running = false;
    int underflow = 0; // 1 = small underflow, 2 = full underflow

    std::vector<AudioDevice> devices;
    double sampleRate = 48000;

    AudioUnit audioUnit = nullptr;
};

class CoreAudioSinkModule : public ModuleManager::Instance {
public:
    CoreAudioSinkModule(std::string name) {
        this->name = name;
        provider.create = create_sink;
        provider.ctx = this;

        sigpath::sinkManager.registerSinkProvider("CoreAudio", provider);
    }

    ~CoreAudioSinkModule() {
        sigpath::sinkManager.unregisterSinkProvider("CoreAudio");
    }

    void postInit() {}

    void enable() {
        enabled = true;
    }

    void disable() {
        enabled = false;
    }

    bool isEnabled() {
        return enabled;
    }

private:
    static SinkManager::Sink* create_sink(SinkManager::Stream* stream, std::string streamName, void* ctx) {
        return (SinkManager::Sink*)(new CoreAudioSink(stream, streamName));
    }

    std::string name;
    bool enabled = true;
    SinkManager::SinkProvider provider;
};

MOD_EXPORT void _INIT_() {
    json def = json({});
    config.setPath(core::args["root"].s() + "/coreaudio_sink_config.json");
    config.load(def);
    config.enableAutoSave();
}

MOD_EXPORT void* _CREATE_INSTANCE_(std::string name) {
    CoreAudioSinkModule* instance = new CoreAudioSinkModule(name);
    return instance;
}

MOD_EXPORT void _DELETE_INSTANCE_(void* instance) {
    delete (CoreAudioSinkModule*)instance;
}

MOD_EXPORT void _END_() {
    config.disableAutoSave();
    config.save();
}
