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
    void audioThread() {
        _mainloop = pa_mainloop_new();
        pa_mainloop_api* api = pa_mainloop_get_api(_mainloop);
        pa_context* context = pa_context_new(api, "SDR++ PulseAudio Sink");

        pa_context_connect(context, NULL, PA_CONTEXT_NOFLAGS, NULL);

        // Wait for context to be ready
        while (_running && pa_context_get_state(context) != PA_CONTEXT_READY) {
            pa_mainloop_iterate(_mainloop, 1, NULL);
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
            pa_mainloop_iterate(_mainloop, 1, NULL);
        }
        pa_operation_unref(op);

        // Main audio loop
        while (_running) {
            if (!_playing) {
                std::this_thread::sleep_for(std::chrono::milliseconds(10));
                continue;
            }

            // Get audio data from packer
            int count = stereoPacker.out.isDataReady() ? stereoPacker.out.read() : -1;
            if (count > 0) {
                std::lock_guard<std::mutex> lock(_audioMutex);
                _audioBuffer.resize(count);
                memcpy(_audioBuffer.data(), stereoPacker.out.readBuf, count * sizeof(dsp::stereo_t));
                stereoPacker.out.flush();
            }

            // Write to PulseAudio
            if (!_audioBuffer.empty()) {
                pa_sample_spec ss = {
                    .format = PA_SAMPLE_FLOAT32LE,
                    .rate = 48000,
                    .channels = 2
                };

                pa_buffer_attr buffer_attr = {
                    .maxlength = (uint32_t)-1,
                    .tlength = 512 * sizeof(dsp::stereo_t),
                    .prebuf = (uint32_t)-1,
                    .minreq = 512 * sizeof(dsp::stereo_t),
                    .fragsize = (uint32_t)-1
                };

                if (!_streamReady) {
                    // Create stream if not exists
                    if (!_paStream) {
                        _paStream = pa_stream_new(context, "SDR++ Audio", &ss, NULL);
                        pa_stream_set_buffer_attr(_paStream, &buffer_attr, NULL, NULL);
                        if (!_paStream) {
                            flog::error("Failed to create PulseAudio stream");
                            return;
                        }
                    
                        // Set stream callbacks
                        pa_stream_set_state_callback(_paStream, [](pa_stream* s, void* userdata) {
                            auto _this = (PulseAudioSink*)userdata;
                            pa_stream_state_t state = pa_stream_get_state(s);
                            flog::info("Stream state changed to {} ({})", 
                                std::to_string(state),
                                state == PA_STREAM_UNCONNECTED ? "UNCONNECTED" :
                                state == PA_STREAM_CREATING ? "CREATING" :
                                state == PA_STREAM_READY ? "READY" :
                                state == PA_STREAM_FAILED ? "FAILED" :
                                state == PA_STREAM_TERMINATED ? "TERMINATED" : "UNKNOWN");
                            
                            if (state == PA_STREAM_READY) {
                                const pa_buffer_attr* buffer_attr = pa_stream_get_buffer_attr(s);
                                if (buffer_attr) {
                                    flog::info("Buffer attributes: maxlength={}, tlength={}, prebuf={}, minreq={}, fragsize={}",
                                        buffer_attr->maxlength,
                                        buffer_attr->tlength,
                                        buffer_attr->prebuf,
                                        buffer_attr->minreq,
                                        buffer_attr->fragsize);
                                }
                                
                                const pa_sample_spec* ss = pa_stream_get_sample_spec(s);
                                if (ss) {
                                    flog::info("Sample spec: format={}, rate={}, channels={}",
                                        (int)ss->format,
                                        (int)ss->rate,
                                        (int)ss->channels);
                                }
                            }
                        }, this);
                    
                        pa_stream_set_write_callback(_paStream, [](pa_stream* s, size_t length, void* userdata) {
                            auto _this = (PulseAudioSink*)userdata;
                            flog::info("Write callback: {} bytes requested", length);
                        }, this);
                    
                        int ret = pa_stream_connect_playback(_paStream, 
                            _deviceName.empty() ? NULL : _deviceName.c_str(), 
                            &buffer_attr, (pa_stream_flags_t)(PA_STREAM_ADJUST_LATENCY | PA_STREAM_AUTO_TIMING_UPDATE), NULL, NULL);
                        if (ret < 0) {
                            flog::error("pa_stream_connect_playback failed: {}", pa_strerror(ret));
                            return;
                        }
                        
                        // Explicitly uncork the stream
                        pa_operation* op = pa_stream_cork(_paStream, 0, NULL, NULL);
                        if (op) {
                            pa_operation_unref(op);
                        }
                    }
                    
                    // Wait for stream to be ready
                    while (pa_stream_get_state(_paStream) != PA_STREAM_READY && _running) {
                        pa_mainloop_iterate(_mainloop, 1, NULL);
                        std::this_thread::sleep_for(std::chrono::milliseconds(10));
                    }
                    _streamReady = true;
                }

                if (_streamReady) {
                    // Write audio data
                    std::lock_guard<std::mutex> lock(_audioMutex);
                    size_t numSamples = _audioBuffer.size();
                    
                    if (_testSineWave) {
                        // Generate test sine wave at 440 Hz
                        const float freq = 440.0f;
                        const float sampleRate = 48000.0f;
                        const float phaseInc = 2.0f * M_PI * freq / sampleRate;
                        
                        for (size_t i = 0; i < numSamples; i++) {
                            float sample = 0.5f * sinf(_testPhase);
                            _audioBuffer[i].l = sample;
                            _audioBuffer[i].r = sample;
                            _testPhase += phaseInc;
                            if (_testPhase > 2.0f * M_PI) {
                                _testPhase -= 2.0f * M_PI;
                            }
                        }
                    }
                    
                    flog::info("Writing {} samples to PulseAudio", numSamples);
                    
                    // Print first few samples for debugging
                    for (int i = 0; i < std::min(4, (int)numSamples); i++) {
                        flog::info("Sample {}: L={} R={}", i, _audioBuffer[i].l, _audioBuffer[i].r);
                    }
                    
                    size_t bytesToWrite = numSamples * sizeof(dsp::stereo_t);
                    flog::info("Attempting to write {} bytes ({} samples) to PulseAudio", bytesToWrite, numSamples);
                    
                    // Check stream state before write
                    pa_stream_state_t state = pa_stream_get_state(_paStream);
                    flog::info("Stream state before write: {}", 
                        state == PA_STREAM_UNCONNECTED ? "UNCONNECTED" :
                        state == PA_STREAM_CREATING ? "CREATING" :
                        state == PA_STREAM_READY ? "READY" :
                        state == PA_STREAM_FAILED ? "FAILED" :
                        state == PA_STREAM_TERMINATED ? "TERMINATED" : "UNKNOWN");

                    // Check if stream is suspended
                    auto suspend_state = pa_stream_is_suspended(_paStream);
                    if (suspend_state) {
                        flog::warn("Stream is suspended - attempting to resume");
                        pa_operation* op = pa_stream_cork(_paStream, 0, NULL, NULL);
                        if (op) pa_operation_unref(op);
                    }

                    // Get sink info to check volume
                    pa_operation* op = pa_context_get_sink_info_by_name(context, 
                        _deviceName.empty() ? NULL : _deviceName.c_str(),
                        [](pa_context* c, const pa_sink_info* i, int eol, void* userdata) {
                            if (eol) return;
                            flog::info("Sink volume: {}", pa_cvolume_avg(&i->volume));
                        }, this);
                    if (op) {
                        while (pa_operation_get_state(op) == PA_OPERATION_RUNNING) {
                            pa_mainloop_iterate(_mainloop, 1, NULL);
                        }
                        pa_operation_unref(op);
                    }

                    int ret = pa_stream_write(_paStream, _audioBuffer.data(), bytesToWrite, NULL, 0, PA_SEEK_RELATIVE);
                    if (ret < 0) {
                        flog::error("pa_stream_write failed: {}", pa_strerror(ret));
                    } else {
                        flog::info("Successfully wrote {} bytes to PulseAudio", bytesToWrite);
                            
                        // Check stream state after write
                        state = pa_stream_get_state(_paStream);
                        flog::info("Stream state after write: {}", 
                            state == PA_STREAM_UNCONNECTED ? "UNCONNECTED" :
                            state == PA_STREAM_CREATING ? "CREATING" :
                            state == PA_STREAM_READY ? "READY" :
                            state == PA_STREAM_FAILED ? "FAILED" :
                            state == PA_STREAM_TERMINATED ? "TERMINATED" : "UNKNOWN");
                            
                        // Check if stream is corked
                        int corked = pa_stream_is_corked(_paStream);
                        if (corked) {
                            flog::warn("Stream is corked - no audio will be played");
                            // Attempt to uncork
                            pa_operation* op = pa_stream_cork(_paStream, 0, NULL, NULL);
                            if (op) pa_operation_unref(op);
                        }

                        // Check if sink is muted
                        pa_operation* op = pa_context_get_sink_info_by_name(context, 
                            _deviceName.empty() ? NULL : _deviceName.c_str(),
                            [](pa_context* c, const pa_sink_info* i, int eol, void* userdata) {
                                if (eol) return;
                                if (i->mute) {
                                    flog::warn("Sink is muted - no audio will be played");
                                }
                            }, this);
                        if (op) {
                            while (pa_operation_get_state(op) == PA_OPERATION_RUNNING) {
                                pa_mainloop_iterate(_mainloop, 1, NULL);
                            }
                            pa_operation_unref(op);
                        }

                        // Check latency
                        pa_usec_t latency;
                        int negative;
                        if (pa_stream_get_latency(_paStream, &latency, &negative) >= 0) {
                            flog::info("Stream latency: {} μs", latency);
                        }
                    }
                    _audioBuffer.clear();
                }
            }
            else {
                if (_streamReady) {
                    // Output silence if no data available
                    std::vector<dsp::stereo_t> silence(512, {0.0f, 0.0f});
                    pa_stream_write(_paStream, silence.data(), silence.size() * sizeof(dsp::stereo_t), NULL, 0, PA_SEEK_RELATIVE);
                }
            }

            std::this_thread::sleep_for(std::chrono::milliseconds(10));
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
        pa_operation* op = pa_context_get_sink_info_list(context, [](pa_context* c, const pa_sink_info* i, int eol, void* userdata) {
            auto _this = (PulseAudioSink*)userdata;
            
            if (eol) return;

            _this->_devices.push_back(i->name);
            if (_this->_deviceName == i->name) {
                _this->_selectedDevice = _this->_devices.size() - 1;
            }
            flog::info("Found audio device: {} ({} channels, {} Hz)", 
                i->name, i->channel_map.channels, i->sample_spec.rate);
        }, this);


        while (pa_operation_get_state(op) == PA_OPERATION_RUNNING) {
            pa_mainloop_iterate(_mainloop, 1, NULL);
            std::this_thread::sleep_for(std::chrono::milliseconds(10));
        }

        pa_operation_unref(op);
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
