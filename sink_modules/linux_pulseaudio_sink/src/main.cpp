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
#include <pulse/pulseaudio.h>

#define CONCAT(a, b) ((std::string(a) + b).c_str())

SDRPP_MOD_INFO{
    /* Name:            */ "linux_pulseaudio_sink",
    /* Description:     */ "PulseAudio sink module for Linux",
    /* Author:          */ "Sanny Sanoff and his aider",
    /* Version:         */ 0, 1, 0,
    /* Max instances    */ 1
};

ConfigManager config;

class PulseAudioSink : SinkManager::Sink {
public:
    struct AudioDevice {
        std::string name;
        std::string description;
        std::vector<double> sampleRates;
        std::string sampleRatesTxt;
    };

    PulseAudioSink(SinkManager::Stream* stream, std::string streamName) {
        _stream = stream;
        _streamName = streamName;
        s2m.init(_stream->sinkOut);
        stereoPacker.init(_stream->sinkOut, 8192);

        // Initialize PulseAudio mainloop
        mainloop = pa_mainloop_new();
        mainloop_api = pa_mainloop_get_api(mainloop);
        context = pa_context_new(mainloop_api, "SDR++ PulseAudio Sink");

        // Connect to PulseAudio server
        pa_context_connect(context, NULL, PA_CONTEXT_NOFLAGS, NULL);

        // Set state callback
        pa_context_set_state_callback(context, [](pa_context* c, void* userdata) {
            PulseAudioSink* _this = (PulseAudioSink*)userdata;
            if (pa_context_get_state(c) == PA_CONTEXT_READY) {
                _this->contextReady = true;
            }
        }, this);

        // Wait for connection with timeout
        int timeout = 100; // 100 iterations = ~1 second
        while (!contextReady && timeout-- > 0) {
            pa_mainloop_iterate(mainloop, 1, NULL);
        }
        if (!contextReady) {
            flog::error("Failed to connect to PulseAudio server");
            return;
        }

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

    ~PulseAudioSink() {
        stop();
        if (stream) {
            pa_stream_disconnect(stream);
            pa_stream_unref(stream);
        }
        if (context) {
            pa_context_disconnect(context);
            pa_context_unref(context);
        }
        if (mainloop) {
            pa_mainloop_free(mainloop);
        }
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

        if (running) {
            doStop();
            doStart();
        }
    }

    void menuHandler() {
        float menuWidth = ImGui::GetContentRegionAvail().x;

        // Device selection
        ImGui::SetNextItemWidth(menuWidth);
        if (ImGui::Combo(("##_pulseaudio_sink_dev_" + _streamName).c_str(), &devId, [](void* data, int idx, const char** out_text) {
            auto devices = (std::vector<AudioDevice>*)data;
            *out_text = devices->at(idx).description.c_str();
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
            if (ImGui::Combo(("##_pulseaudio_sink_sr_" + _streamName).c_str(), &srId, devices[devId].sampleRatesTxt.c_str())) {
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
        
        // Create stream
        pa_sample_spec ss = {
            .format = PA_SAMPLE_FLOAT32LE,
            .rate = (uint32_t)sampleRate,
            .channels = 2
        };

        stream = pa_stream_new(context, "SDR++ Audio", &ss, NULL);
        if (!stream) {
            flog::error("Could not create PulseAudio stream");
            return false;
        }

        // Set stream callbacks
        pa_stream_set_state_callback(stream, [](pa_stream* s, void* userdata) {
            PulseAudioSink* _this = (PulseAudioSink*)userdata;
            if (pa_stream_get_state(s) == PA_STREAM_READY) {
                _this->streamReady = true;
            }
        }, this);

        pa_stream_set_write_callback(stream, [](pa_stream* s, size_t length, void* userdata) {
            ((PulseAudioSink*)userdata)->writeCallback(length);
        }, this);

        // Connect stream to device
        pa_buffer_attr buffer_attr = {
            .maxlength = (uint32_t)(sampleRate * 0.1 * sizeof(dsp::stereo_t)), // 100ms buffer
            .tlength = (uint32_t)(sampleRate * 0.05 * sizeof(dsp::stereo_t)), // 50ms target
            .prebuf = (uint32_t)-1,
            .minreq = (uint32_t)(sampleRate * 0.01 * sizeof(dsp::stereo_t)), // 10ms minimum
            .fragsize = (uint32_t)(sampleRate * 0.01 * sizeof(dsp::stereo_t)) // 10ms fragments
        };

        int flags = PA_STREAM_ADJUST_LATENCY | PA_STREAM_AUTO_TIMING_UPDATE | PA_STREAM_INTERPOLATE_TIMING;
        if (pa_stream_connect_playback(stream, dev.name.c_str(), &buffer_attr, (pa_stream_flags_t)flags, NULL, NULL) < 0) {
            flog::error("Could not connect PulseAudio stream");
            return false;
        }

        // Wait for stream to be ready with timeout
        int timeout = 100; // 100 iterations = ~1 second
        while (!streamReady && timeout-- > 0) {
            pa_mainloop_iterate(mainloop, 1, NULL);
        }
        if (!streamReady) {
            flog::error("Failed to initialize PulseAudio stream");
            return false;
        }

        stereoPacker.start();
        return true;
    }

    void doStop() {
        if (stream) {
            pa_stream_disconnect(stream);
        }
        stereoPacker.stop();
    }

    void writeCallback(size_t length) {
        // Ensure we're in the main thread
        if (!gui::mainWindow.isPlaying()) {
            // Write silence
            void* data;
            size_t size = length;
            if (pa_stream_begin_write(stream, &data, &size) == 0) {
                memset(data, 0, size);
                pa_stream_write(stream, data, size, NULL, 0, PA_SEEK_RELATIVE);
            }
            return;
        }

        // Process audio data in chunks
        size_t remaining = length;
        while (remaining > 0) {
            void* data;
            size_t chunkSize = remaining;
            if (pa_stream_begin_write(stream, &data, &chunkSize) != 0) {
                break;
            }

            if (stereoPacker.out.isDataReady()) {
                int count = stereoPacker.out.read();
                if (count > 0) {
                    size_t actualSize = std::min(chunkSize, (size_t)count * sizeof(dsp::stereo_t));
                    memcpy(data, stereoPacker.out.readBuf, actualSize);
                    stereoPacker.out.flush();
                    pa_stream_write(stream, data, actualSize, NULL, 0, PA_SEEK_RELATIVE);
                    remaining -= actualSize;
                    continue;
                }
            }
            
            // If no data available, write silence
            memset(data, 0, chunkSize);
            pa_stream_write(stream, data, chunkSize, NULL, 0, PA_SEEK_RELATIVE);
            remaining -= chunkSize;
        }
    }

    void enumerateDevices() {
        pa_operation* op = pa_context_get_sink_info_list(context, [](pa_context* c, const pa_sink_info* i, int eol, void* userdata) {
            auto _this = (PulseAudioSink*)userdata;
            
            if (eol) { return; }

            AudioDevice dev;
            dev.name = i->name;
            dev.description = i->description;

            // Get supported sample rates
            dev.sampleRates = { 44100, 48000, 96000, 192000 }; // PulseAudio typically supports these
            for (auto sr : dev.sampleRates) {
                dev.sampleRatesTxt += std::to_string((int)sr);
                dev.sampleRatesTxt += '\0';
            }

            _this->devices.push_back(dev);
        }, this);

        while (pa_operation_get_state(op) == PA_OPERATION_RUNNING) {
            pa_mainloop_iterate(mainloop, 1, NULL);
        }
        pa_operation_unref(op);
    }

    SinkManager::Stream* _stream;
    dsp::convert::StereoToMono s2m;
    dsp::buffer::Packer<dsp::stereo_t> stereoPacker;

    std::string _streamName;

    int srId = 0;
    int devId = -1;
    bool running = false;
    int underflow = 0;

    std::vector<AudioDevice> devices;
    double sampleRate = 48000;

    pa_mainloop* mainloop = nullptr;
    pa_mainloop_api* mainloop_api = nullptr;
    pa_context* context = nullptr;
    pa_stream* stream = nullptr;
    bool contextReady = false;
    bool streamReady = false;
};

class PulseAudioSinkModule : public ModuleManager::Instance {
public:
    PulseAudioSinkModule(std::string name) {
        this->name = name;
        provider.create = create_sink;
        provider.ctx = this;
        sigpath::sinkManager.registerSinkProvider("PulseAudio", provider);
    }

    ~PulseAudioSinkModule() {
        sigpath::sinkManager.unregisterSinkProvider("PulseAudio");
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
        return (SinkManager::Sink*)(new PulseAudioSink(stream, streamName));
    }

    std::string name;
    bool enabled = true;
    SinkManager::SinkProvider provider;
};

MOD_EXPORT void _INIT_() {
    json def = json({});
    config.setPath(core::args["root"].s() + "/pulseaudio_sink_config.json");
    config.load(def);
    config.enableAutoSave();
}

MOD_EXPORT void* _CREATE_INSTANCE_(std::string name) {
    PulseAudioSinkModule* instance = new PulseAudioSinkModule(name);
    return instance;
}

MOD_EXPORT void _DELETE_INSTANCE_(void* instance) {
    delete (PulseAudioSinkModule*)instance;
}

MOD_EXPORT void _END_() {
    config.disableAutoSave();
    config.save();
}
