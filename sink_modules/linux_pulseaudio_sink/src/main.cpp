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
#include <atomic>
#include <mutex>
#include <vector>
#include <thread>

#define CONCAT(a, b) ((std::string(a) + b).c_str())

SDRPP_MOD_INFO{
    /* Name:            */ "linux_pulseaudio_sink",
    /* Description:     */ "PulseAudio sink module for Linux",
    /* Author:          */ "Sanny Sanoff and his aider",
    /* Version:         */ 0, 1, 0,
    /* Max instances    */ 1
};

ConfigManager config;

#define DEFAULT_SAMPLE_RATE 48000

class PulseAudioSink : SinkManager::Sink {
public:
    PulseAudioSink(SinkManager::Stream* stream, std::string streamName) :
        _stream(stream), _streamName(streamName) {

        // Initialize packer
        stereoPacker.init(_stream->sinkOut, 512);

        // Testing flag for sine wave generation
        _testSineWave = false;
        _testPhase = 0.0f;

        // Load config
        config.acquire();
        if (!config.conf.contains(_streamName)) {
            config.conf[_streamName]["device"] = "";
        }
        _deviceName = config.conf[_streamName]["device"];
        config.release(true);

        // Start audio thread
        _running = true;
        _audioThread = std::thread(&PulseAudioSink::audioThread, this);
    }

    ~PulseAudioSink() {
        stop();
        if (_audioThread.joinable()) {
            _running = false;
            _audioThread.join();
        }
    }

    void start() {
        if (_playing) return;
        _stream->setSampleRate(DEFAULT_SAMPLE_RATE);
        _playing = true;
        stereoPacker.start();
    }

    void stop() {
        if (!_playing) return;
        _playing = false;
        stereoPacker.stop();
    }

    void menuHandler() {
        float menuWidth = ImGui::GetContentRegionAvail().x;

        ImGui::SetNextItemWidth(menuWidth);
        if (ImGui::Combo(("##_pulseaudio_sink_dev_" + _streamName).c_str(), &_selectedDevice, [](void* data, int idx, const char** out_text) {
            auto devices = (std::vector<std::string>*)data;
            *out_text = devices->at(idx).c_str();
            return true;
        }, &_devices, _devices.size())) {
            _deviceName = _devices[_selectedDevice];
            config.acquire();
            config.conf[_streamName]["device"] = _deviceName;
            config.release(true);
        }

        // Add test sine wave toggle
        if (ImGui::Checkbox("Test Sine Wave", &_testSineWave)) {
            _testPhase = 0.0f; // Reset phase when toggling
        }
    }

private:

    int zzz;

    void paThreadFunc() {
        pa_mainloop_run(_mainloop, NULL);
    }

    void audioThread() {
        _mainloop = pa_mainloop_new();
        pa_mainloop_api* api = pa_mainloop_get_api(_mainloop);
        pa_context* context = pa_context_new(api, "SDR++ PulseAudio Sink");
        pa_context_connect(context, NULL, PA_CONTEXT_NOFLAGS, NULL);

        // Start mainloop in separate thread
        std::thread paThread(&PulseAudioSink::paThreadFunc, this);

        // Wait for context to be ready
        while (_running && pa_context_get_state(context) != PA_CONTEXT_READY) {
            std::this_thread::sleep_for(std::chrono::milliseconds(10));
        }

        // Enumerate devices and log detailed info
        enumerateDevices(context);

        // Log current default sink
        pa_operation* op = pa_context_get_server_info(context, [](pa_context* c, const pa_server_info* i, void* userdata) {
            auto _this = (PulseAudioSink*)userdata;
            flog::info("PulseAudio server info:");
            flog::info("  Default sink: {}", i->default_sink_name);
            flog::info("  Sample spec: {} Hz, {} channels", i->sample_spec.rate, i->sample_spec.channels);
        }, this);

        while (pa_operation_get_state(op) == PA_OPERATION_RUNNING) {
            std::this_thread::sleep_for(std::chrono::milliseconds(10));
        }
        pa_operation_unref(op);

        _streamReady = false;

        // Main audio loop
        while (_running) {
            if (!_playing) {
                std::this_thread::sleep_for(std::chrono::milliseconds(10));
                continue;
            }

            // Let packer process data
            //stereoPacker.process();
        }

        // Clean up
        if (_paStream) {
            pa_stream_disconnect(_paStream);
            pa_stream_unref(_paStream);
            _paStream = nullptr;
        }
        pa_context_disconnect(context);
        pa_context_unref(context);
        if (_mainloop) {
            pa_mainloop_free(_mainloop);
            _mainloop = nullptr;
        }
    }

    void enumerateDevices(pa_context* context) {
        pa_operation* op = pa_context_get_sink_info_list(context,
            [](pa_context* c, const pa_sink_info* i, int eol, void* userdata) {
                PulseAudioSink* _this = static_cast<PulseAudioSink*>(userdata);
                if(eol) return;
                _this->_devices.push_back(i->name);
                if(i->name == _this->_deviceName) {
                    _this->_selectedDevice = _this->_devices.size() - 1;
                }
            }, this);

        while (pa_operation_get_state(op) == PA_OPERATION_RUNNING) {
            std::this_thread::sleep_for(std::chrono::milliseconds(10));
        }

        pa_operation_unref(op);
    }

    bool createStream(pa_context* context) {
         pa_sample_spec ss = {
             .format = PA_SAMPLE_FLOAT32LE,
             .rate = DEFAULT_SAMPLE_RATE,
             .channels = 2
         };

         pa_buffer_attr buffer_attr = {
             .maxlength = (uint32_t)-1,
             .tlength = 512 * sizeof(dsp::stereo_t),
             .prebuf = (uint32_t)-1,
             .minreq = 512 * sizeof(dsp::stereo_t),
             .fragsize = (uint32_t)-1
         };

         if (!_paStream) {
             _paStream = pa_stream_new(context, "SDR++ Audio", &ss, NULL);
             if (!_paStream) {
                 flog::error("Failed to create PulseAudio stream");
                 return false;
             }
             pa_stream_set_buffer_attr(_paStream, &buffer_attr, NULL, NULL);

             pa_stream_set_write_callback(_paStream, [](pa_stream* s, size_t length, void* userdata) {
                PulseAudioSink* _this = static_cast<PulseAudioSink*>(userdata);
                size_t available = _this->stereoPacker.out.isDataReady() ? _this->stereoPacker.out.read() : -1;

                if(available > 0) {
                    void* data;
                    pa_stream_begin_write(s, &data, &length);
                    size_t toWrite = std::min(available, length/sizeof(dsp::stereo_t));

                    if(toWrite > 0) {
                        memcpy(data, _this->stereoPacker.out.readBuf, available * sizeof(dsp::stereo_t));
                        //int rd = _this->stereoPacker.out.read((dsp::stereo_t*)data, toWrite);
                        pa_stream_write(s, data, toWrite * sizeof(dsp::stereo_t), NULL, 0, PA_SEEK_RELATIVE); // dropping non-written data
                    }
                    _this->stereoPacker.out.flush();
                }
            }, this);

            pa_stream_set_state_callback(_paStream, [](pa_stream* s, void* userdata) {
                // Handle state changes
            }, this);

             int ret = pa_stream_connect_playback(_paStream,
                 _deviceName.empty() ? NULL : _deviceName.c_str(),
                 &buffer_attr, (pa_stream_flags_t)(PA_STREAM_ADJUST_LATENCY | PA_STREAM_AUTO_TIMING_UPDATE), NULL, NULL);
             if (ret < 0) {
                 flog::error("pa_stream_connect_playback failed: {}", pa_strerror(ret));
                 return false;
             }

             pa_operation* op = pa_stream_cork(_paStream, 0, NULL, NULL);
             if (op) {
                 pa_operation_unref(op);
             }
         }
         return true;
    }

    SinkManager::Stream* _stream;
    dsp::buffer::Packer<dsp::stereo_t> stereoPacker;

    std::string _streamName;
    std::string _deviceName;
    std::vector<std::string> _devices;
    int _selectedDevice = 0;

    std::atomic<bool> _running{false};
    std::atomic<bool> _playing{false};
    std::thread _audioThread;

    std::mutex _audioMutex;
    std::vector<dsp::stereo_t> _audioBuffer;
    pa_mainloop* _mainloop = nullptr;
    pa_stream* _paStream = nullptr;
    bool _streamReady = false;
    bool _volumeSet = false;

    // Testing variables
    bool _testSineWave;
    float _testPhase;
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
