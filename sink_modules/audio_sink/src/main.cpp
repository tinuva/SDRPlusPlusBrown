#define _USE_MATH_DEFINES
#include <cmath>
#include <imgui.h>
#include <module.h>
#include <gui/gui.h>
#include <signal_path/signal_path.h>
#include <signal_path/sink.h>
#include <dsp/buffer/packer.h>
#include <dsp/convert/stereo_to_mono.h>
#include <utils/flog.h>
#include <RtAudio.h>
#include <config.h>
#include <core.h>

#define CONCAT(a, b) ((std::string(a) + b).c_str())

SDRPP_MOD_INFO{
    /* Name:            */ "audio_sink",
    /* Description:     */ "Audio sink module for SDR++",
    /* Author:          */ "Ryzerth",
    /* Version:         */ 0, 1, 0,
    /* Max instances    */ 1
};

ConfigManager config;

static bool rtaudioCallbackError;
static void rtaudioCallback(RtAudioError::Type type, const std::string& errorText) {
    rtaudioCallbackError = true;
}


class AudioSink : SinkManager::Sink {
public:
    RtAudio::DeviceInfo inputDeviceInfo;

    std::vector<dsp::stereo_t> playBuffer; // hardware data is stored here before stream i/o

    AudioSink(SinkManager::Stream* stream, std::string streamName) {
        _stream = stream;
        _streamName = streamName;
        s2m.init(_stream->sinkOut);
        monoPacker.init(&s2m.out, 512);
        stereoPacker.init(_stream->sinkOut, 512);

        bool created = false;
        std::string device = "";
        config.acquire();
        if (!config.conf.contains(_streamName)) {
            created = true;
            config.conf[_streamName]["device"] = "";
            config.conf[_streamName]["devices"] = json({});
        }
        device = config.conf[_streamName]["device"];
        config.release(created);

        int count = audio.getDeviceCount();
        RtAudio::DeviceInfo info;
        for (int i = 0; i < count; i++) {
            info = audio.getDeviceInfo(i);
            if (info.isDefaultInput) {
                inputDeviceInfo = info;
                defaultInputDeviceId = i;
                flog::info("Default input: {} defaultInputDeviceId={}", info.name, std::to_string(i));
            }
            if (!info.probed) {
                RtAudio::StreamParameters parameters;
                parameters.deviceId = i;
                parameters.nChannels = 2;
                unsigned int bufferFrames = sampleRate / 60;
                RtAudio::StreamOptions opts;
//                opts.flags = RTAUDIO_MINIMIZE_LATENCY;
                opts.streamName = _streamName;
                info.probed = true;
                rtaudioCallbackError = false;
                try {
                    audio.openStream(&parameters, NULL, RTAUDIO_FLOAT32, sampleRate, &bufferFrames, &callback, this, &opts, rtaudioCallback);
                } catch (RtAudioError &err) {
                    rtaudioCallbackError = true;
                }
                if (rtaudioCallbackError) {
                    continue;
                }
                audio.closeStream();
                info.outputChannels = 2;
            }
            if (info.outputChannels == 0) { continue; }
            if (info.isDefaultOutput) { defaultOutputDevId = devList.size(); }
            devList.push_back(info);
            txtDevList += info.name;
            txtDevList += '\0';
            auto ni = info;
            ni.name += " -> left";
            devList.push_back(ni);
            txtDevList += ni.name;
            txtDevList += '\0';
            ni = info;
            ni.name += " -> right";
            devList.push_back(ni);
            txtDevList += ni.name;
            txtDevList += '\0';
            deviceIds.push_back(i);
            deviceIds.push_back(i);
            deviceIds.push_back(i);

        }

        selectByName(device);
    }

    ~AudioSink() {
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
        selectById(defaultOutputDevId);
    }

    void selectByName(std::string name) {
        for (int i = 0; i < devList.size(); i++) {
            if (devList[i].name == name) {
                selectById(i);
                return;
            }
        }
        selectFirst();
    }

    void selectById(int id) {
        devId = id;
        bool created = false;
        config.acquire();
        if (!config.conf[_streamName]["devices"].contains(devList[id].name)) {
            created = true;
            config.conf[_streamName]["devices"][devList[id].name] = devList[id].preferredSampleRate;
        }
        sampleRate = config.conf[_streamName]["devices"][devList[id].name];
        config.release(created);

        sampleRates = devList[id].sampleRates;
        sampleRatesTxt = "";
        char buf[256];
        bool found = false;
        unsigned int defaultId = 0;
        unsigned int defaultSr = devList[id].preferredSampleRate;
        for (int i = 0; i < sampleRates.size(); i++) {
            if (sampleRates[i] == sampleRate) {
                found = true;
                srId = i;
            }
            if (sampleRates[i] == defaultSr) {
                defaultId = i;
            }
            sprintf(buf, "%d", sampleRates[i]);
            sampleRatesTxt += buf;
            sampleRatesTxt += '\0';
        }
        if (!found) {
            sampleRate = defaultSr;
            srId = defaultId;
        }

        _stream->setSampleRate(sampleRate);

        if (running) { doStop(); }
        if (running) { doStart(); }
    }

    void menuHandler() {
        float menuWidth = ImGui::GetContentRegionAvail().x;

        ImGui::SetNextItemWidth(menuWidth);
        if (ImGui::Combo(("##_audio_sink_dev_" + _streamName).c_str(), &devId, txtDevList.c_str())) {
            selectById(devId);
            config.acquire();
            config.conf[_streamName]["device"] = devList[devId].name;
            config.release(true);
        }

        if (SinkManager::getSecondaryStreamIndex(_streamName).second == 0) {
            // only primary one has frequency selection
            if (ImGui::Combo(("##_audio_sink_sr_" + _streamName).c_str(), &srId, sampleRatesTxt.c_str())) {
                sampleRate = sampleRates[srId];
                _stream->setSampleRate(sampleRate);
                if (running) {
                    doStop();
                    doStart();
                }
                config.acquire();
                config.conf[_streamName]["devices"][devList[devId].name] = sampleRate;
                config.release(true);
            }
        }
    }

    const int microFrames = 4;

private:
    bool doStart() {

        flog::info("Starting RtAudio streams..");
        RtAudio::StreamParameters inputParameters;
        inputParameters.deviceId = defaultInputDeviceId;
        inputParameters.nChannels = 1;

        {
            RtAudio::StreamParameters outputParameters;
            outputParameters.deviceId = deviceIds[devId];
            outputParameters.nChannels = 2;
            unsigned int bufferFrames = sampleRate / 60;
            playBuffer.reserve(bufferFrames);
            RtAudio::StreamOptions opts;
            opts.flags = RTAUDIO_MINIMIZE_LATENCY;
            opts.streamName = _streamName;
            std::replace(opts.streamName.begin(), opts.streamName.end(), '#', '_');
            flog::info("Starting RtAudio stream {}  parameters.deviceId={}  it is default input? {}", _streamName, outputParameters.deviceId, defaultInputDeviceId == outputParameters.deviceId);

            try {
                unsigned int microBuffer = bufferFrames/microFrames;
                audio.openStream(&outputParameters, defaultInputDeviceId == outputParameters.deviceId ? &inputParameters : nullptr, RTAUDIO_FLOAT32, sampleRate, &microBuffer, &callback, this, &opts);
                stereoPacker.setSampleCount((int)bufferFrames);
                audio.startStream();
                stereoPacker.start();
            }
            catch (RtAudioError& e) {
                flog::error("Could not open audio device: {}",  e.getMessage());
                return false;
            }

            flog::info("RtAudio output stream open");
        }
        if (SinkManager::getSecondaryStreamIndex(_streamName).second == 0) {
            if (defaultInputDeviceId != deviceIds[devId] && defaultInputDeviceId != -1) {
                // input device differs from output
                RtAudio::StreamOptions opts;
                opts.flags = RTAUDIO_MINIMIZE_LATENCY;
                opts.streamName = inputDeviceInfo.name;
                flog::info("Starting (separately) RtAudio INPUT stream {}  parameters.deviceId={} (output was: {})", inputDeviceInfo.name, defaultInputDeviceId, deviceIds[devId]);
                unsigned int bufferFrames = sampleRate / 60 / microFrames;

                try {
                    audio2.openStream(nullptr, &inputParameters, RTAUDIO_FLOAT32, sampleRate, &bufferFrames, &callback2, this, &opts);
                    audio2.startStream();
                    flog::info("RtAudio input stream open");
                }
                catch (RtAudioError& e) {
                    flog::error("Could not open INPUT audio device: {}", e.getMessage());
                    return false;
                }
            }
            flog::info("sigpath::sinkManager.defaultInputAudio.init(microphone)");
            sigpath::sinkManager.defaultInputAudio.setInput(&microphone);
            sigpath::sinkManager.defaultInputAudio.start();
            microphone.setBufferSize(sampleRate / 60);
        }
        return true;
    }

    dsp::stream<dsp::stereo_t> microphone;


    void doStop() {
        flog::info("Stopping RtAudio stream:  {}", _streamName);

        sigpath::sinkManager.defaultInputAudio.stop();
        flog::info("sigpath::sinkManager.defaultInputAudio.setInput(nullptr)");
        sigpath::sinkManager.defaultInputAudio.setInput(nullptr);

        s2m.stop();
        monoPacker.stop();
        stereoPacker.stop();
        monoPacker.out.stopReader();
        stereoPacker.out.stopReader();
        if (audio2.isStreamRunning()) {
            flog::info("Stopping RtAudio-2 stream p.3");
            audio2.stopStream();
            flog::info("Stopped RtAudio stream p.3");
        }
        if (audio2.isStreamOpen()) {
            flog::info("Stopping RtAudio-2 stream p.4");
            audio2.closeStream();
            flog::info("Stopped RtAudio stream p.4");
        }
        if (audio.isStreamRunning()) {
            flog::info("Stopping RtAudio-1 stream p.1");
            audio.stopStream();
            flog::info("Stopped RtAudio stream p.1");
        }
        if (audio.isStreamOpen()) {
            flog::info("Stopping RtAudio-1 stream p.2");
            audio.closeStream();
            flog::info("Stopped RtAudio stream p.2");
        }
        monoPacker.out.clearReadStop();
        stereoPacker.out.clearReadStop();
    }

    static int callback2(void* outputBuffer, void* inputBuffer, unsigned int nBufferFrames, double streamTime, RtAudioStreamStatus status, void* userData) {
        AudioSink* _this = (AudioSink*)userData;
        if (inputBuffer != nullptr) {
//            static int counter = 0;
//            if (counter++ % 30 == 0) {
//                float* ib = (float*)inputBuffer;
//                printf("ok here input buffer2 %d: %f %f %f %f %f %f %f %f\n", nBufferFrames, ib[0], ib[1], ib[2], ib[3], ib[4], ib[5], ib[6], ib[7]);
//            }
            memmove(_this->microphone.writeBuf, inputBuffer, nBufferFrames * 2 * sizeof(float));
            _this->microphone.swap(nBufferFrames);
        }
        return 0;
    }

    static int callback(void* outputBuffer, void* inputBuffer, unsigned int nBufferFrames, double streamTime, RtAudioStreamStatus status, void* userData) {

        if (inputBuffer != nullptr) {
            static int counter = 0;
            if (counter++ % 30 == 0) {
//                float* ib = (float*)inputBuffer;
//                printf("ok here input buffer: %f %f %f %f %f %f %f %f\n", ib[0], ib[1], ib[2], ib[3], ib[4], ib[5], ib[6], ib[7]);
            }
        }

        AudioSink* _this = (AudioSink*)userData;
        if (_this->playBuffer.size() < nBufferFrames) {
            int count = _this->stereoPacker.out.read(); // something has to be here
            if (count > 0) {
                int oldSize = _this->playBuffer.size();
                _this->playBuffer.resize(_this->playBuffer.size() + count);
                memmove(_this->playBuffer.data() + oldSize, _this->stereoPacker.out.readBuf, count * sizeof(dsp::stereo_t));
            }
            _this->stereoPacker.out.flush();
        }

        if (_this->playBuffer.size() >= nBufferFrames) {

            // For debug purposes only...
            // if (nBufferFrames != count) { flog::warn("Buffer size mismatch, wanted {0}, was asked for {1}", count, nBufferFrames); }
            // for (int i = 0; i < count; i++) {
            //     if (_this->stereoPacker.out.readBuf[i].l == NAN || _this->stereoPacker.out.readBuf[i].r == NAN) { flog::error("NAN in audio data"); }
            //     if (_this->stereoPacker.out.readBuf[i].l == INFINITY || _this->stereoPacker.out.readBuf[i].r == INFINITY) { flog::error("INFINITY in audio data"); }
            //     if (_this->stereoPacker.out.readBuf[i].l == -INFINITY || _this->stereoPacker.out.readBuf[i].r == -INFINITY) { flog::error("-INFINITY in audio data"); }
            // }


            memcpy(outputBuffer, _this->playBuffer.data(), nBufferFrames * sizeof(dsp::stereo_t));
            _this->playBuffer.erase(_this->playBuffer.begin(), _this->playBuffer.begin() + nBufferFrames);

            static float lastPhase = 0;
            auto stereoOut = (dsp::stereo_t*)outputBuffer;
            switch (sigpath::sinkManager.toneGenerator.load()) {
            case 1:
                float hz = 800;
                int period = (int)(_this->sampleRate / hz);
                float tick = M_PI * 2 / period;
                for(int q=0; q<nBufferFrames; q++) {
                    stereoOut[q].r = stereoOut[q].l = sin(lastPhase);
                    lastPhase += tick;
                }
            }
            auto channel = _this->devId % 3; // 0=stereo 1=left 2=right
            switch (channel) {
            default:
                break;
            case 1: // left
                for (int i = 0; i < nBufferFrames; i++) {
                    stereoOut[i].r = 0;
                }
                break;
            case 2: // right
                for (int i = 0; i < nBufferFrames; i++) {
                    stereoOut[i].l = 0;
                }
                break;
            }
        }
        return 0;
    }

    SinkManager::Stream* _stream;
    dsp::convert::StereoToMono s2m;
    dsp::buffer::Packer<float> monoPacker;
    dsp::buffer::Packer<dsp::stereo_t> stereoPacker;

    std::string _streamName;

    int srId = 0;
    int devCount;
    int devId = 0;
    bool running = false;

    unsigned int defaultOutputDevId = 0;         // dev == index in our reduced list
    unsigned int defaultInputDeviceId = -1;       // device = index in the rtaudio devices

    std::vector<RtAudio::DeviceInfo> devList;
    std::vector<unsigned int> deviceIds;
    std::string txtDevList;

    std::vector<unsigned int> sampleRates;
    std::string sampleRatesTxt;
    unsigned int sampleRate = 48000;

    RtAudio audio;
    RtAudio audio2;
};

class AudioSinkModule : public ModuleManager::Instance {
public:
    AudioSinkModule(std::string name) {
        this->name = name;
        provider.create = create_sink;
        provider.ctx = this;

        sigpath::sinkManager.registerSinkProvider("Audio", provider);
    }

    ~AudioSinkModule() {
        // Unregister sink, this will automatically stop and delete all instances of the audio sink
        sigpath::sinkManager.unregisterSinkProvider("Audio");
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
        return (SinkManager::Sink*)(new AudioSink(stream, streamName));
    }

    std::string name;
    bool enabled = true;
    SinkManager::SinkProvider provider;
};

MOD_EXPORT void _INIT_() {
    json def = json({});
    config.setPath(core::args["root"].s() + "/audio_sink_config.json");
    config.load(def);
    config.enableAutoSave();
}

MOD_EXPORT void* _CREATE_INSTANCE_(std::string name) {
    AudioSinkModule* instance = new AudioSinkModule(name);
    return instance;
}

MOD_EXPORT void _DELETE_INSTANCE_(void* instance) {
    delete (AudioSinkModule*)instance;
}

MOD_EXPORT void _END_() {
    config.disableAutoSave();
    config.save();
}