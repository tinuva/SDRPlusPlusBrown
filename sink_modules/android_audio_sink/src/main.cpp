#include <imgui.h>
#include <module.h>
#include <gui/gui.h>
#include <signal_path/signal_path.h>
#include <signal_path/sink.h>
#include <dsp/buffer/packer.h>
#include <spdlog/spdlog.h>
#include <config.h>
#include <utils/optionlist.h>
#include <aaudio/AAudio.h>
#include <core.h>
#include <android/log.h>
#include <jni.h>
#include <android_native_app_glue.h>
#include <spdlog/sinks/android_sink.h>

#define CONCAT(a, b) ((std::string(a) + b).c_str())

SDRPP_MOD_INFO{
    /* Name:            */ "audio_sink",
    /* Description:     */ "Android audio sink module for SDR++",
    /* Author:          */ "Ryzerth",
    /* Version:         */ 0, 1, 0,
    /* Max instances    */ 1
};

ConfigManager config;

namespace backend {
    struct android_app* app = NULL;
}

class AudioSink : SinkManager::Sink {

    JNIEnv* java_env = NULL;

public:
    AudioSink(SinkManager::Stream* stream, std::string streamName) {
        _stream = stream;
        _streamName = streamName;

        packer.init(_stream->sinkOut, 512);

        config.acquire();
        if (config.conf.find("useRawInput") != config.conf.end()) {
            this->useRawInput = config.conf["useRawInput"];
        }
        config.release(true);

        // TODO: Add choice? I don't think anyone cares on android...
        sampleRate = 48000;
        _stream->setSampleRate(sampleRate);
        auto console_sink = std::make_shared<spdlog::sinks::android_sink_st>("SDR++/android_audio_sink");
        auto logger = std::shared_ptr<spdlog::logger>(new spdlog::logger("", { console_sink }));
        spdlog::set_default_logger(logger);

    }

    ~AudioSink() {
    }

    void start() {
        if (running) {
            return;
        }
        doStart();
        running = true;
    }

    void stop() {
        if (!running) {
            return;
        }
        doStop();
        running = false;
    }

    void menuHandler() {
        // Draw menu here
        if (ImGui::Checkbox("Use Raw Input ", &this->useRawInput)) {
            config.acquire();
            config.conf["useRawInput"] = this->useRawInput;
            config.release(true);
            restart();
        }
    }

private:
    dsp::stream<dsp::stereo_t> microphone;

    void doStart() {
        bufferSize = round(sampleRate / 60.0);
        sigpath::sinkManager.defaultInputAudio.init(&microphone);
        sigpath::sinkManager.defaultInputAudio.start();
        microphone.setBufferSize(sampleRate / 60);

        {

            // Create stream builder
            AAudioStreamBuilder *builder;
            aaudio_result_t result = AAudio_createStreamBuilder(&builder);
            if (result == 0) {
                // Set stream options
                if (this->useRawInput) {
                    AAudioStreamBuilder_setInputPreset(builder, AAUDIO_INPUT_PRESET_VOICE_PERFORMANCE);
                } else {
                    AAudioStreamBuilder_setInputPreset(builder,
                                                       AAUDIO_INPUT_PRESET_VOICE_COMMUNICATION);
                }
                AAudioStreamBuilder_setDirection(builder, AAUDIO_DIRECTION_INPUT);
                AAudioStreamBuilder_setSharingMode(builder, AAUDIO_SHARING_MODE_SHARED);
                AAudioStreamBuilder_setSampleRate(builder, sampleRate);
                AAudioStreamBuilder_setChannelCount(builder, 2);
                AAudioStreamBuilder_setFormat(builder, AAUDIO_FORMAT_PCM_FLOAT);
                AAudioStreamBuilder_setBufferCapacityInFrames(builder, bufferSize);
                AAudioStreamBuilder_setErrorCallback(builder, errorCallback, this);

                // Open the stream
                result = AAudioStreamBuilder_openStream(builder, &streamW);
                switch(result) {
                    case AAUDIO_ERROR_INTERNAL:
                        break;
                }
                if (result == 0) {

                    // Stream stream and packer
                    AAudioStream_requestStart(streamW);
                }

                    // We no longer need the builder
                AAudioStreamBuilder_delete(builder);
            }

            if (result == 0) {
                // Start worker thread
                workerThreadW = std::thread(&AudioSink::workerW, this);
            }
        }
        {
            // Create stream builder
            AAudioStreamBuilder *builder;
            aaudio_result_t result = AAudio_createStreamBuilder(&builder);

            // Set stream options
            AAudioStreamBuilder_setDirection(builder, AAUDIO_DIRECTION_OUTPUT);
            AAudioStreamBuilder_setSharingMode(builder, AAUDIO_SHARING_MODE_SHARED);
            AAudioStreamBuilder_setSampleRate(builder, sampleRate);
            AAudioStreamBuilder_setChannelCount(builder, 2);
            AAudioStreamBuilder_setFormat(builder, AAUDIO_FORMAT_PCM_FLOAT);
            AAudioStreamBuilder_setBufferCapacityInFrames(builder, bufferSize);
            AAudioStreamBuilder_setErrorCallback(builder, errorCallback, this);
            packer.setSampleCount(bufferSize);

            // Open the stream
            AAudioStreamBuilder_openStream(builder, &stream);

            // Stream stream and packer
            packer.start();
            AAudioStream_requestStart(stream);

            // We no longer need the builder
            AAudioStreamBuilder_delete(builder);

            // Start worker thread
            workerThread = std::thread(&AudioSink::worker, this);
        }

    }

    void doStop() {
        packer.stop();
        packer.out.stopReader();
        bool streamWExists = streamW != nullptr;
        streamWMutex.lock();
        if (streamW) {
            AAudioStream_requestStop(streamW);
        }
        AAudioStream_requestStop(stream);
        streamWMutex.unlock();
        usleep(200000);
        streamWMutex.lock();
        if (streamW)
            AAudioStream_close(streamW);
        AAudioStream_close(stream);
        streamWMutex.unlock();
        usleep(200000);
        streamWMutex.lock();
        streamW = nullptr;
        streamWMutex.unlock();
        if (workerThread.joinable()) { workerThread.join(); }
        if (streamWExists) {
            if (workerThreadW.joinable()) { workerThreadW.join(); }
        }
        packer.out.clearReadStop();

        sigpath::sinkManager.defaultInputAudio.stop();
        sigpath::sinkManager.defaultInputAudio.setInput(nullptr);

    }

    void worker() {
        while (true) {
            int count = packer.out.read();
            if (count < 0) { return; }
            AAudioStream_write(stream, packer.out.readBuf, count, 100000000); // 100 msec
            packer.out.flush();
        }
    }

    void workerW() {
        std::vector<dsp::stereo_t> samplesBuffer;
        samplesBuffer.resize(bufferSize);
        int nInBuffer = 0;
        while (true) {
            if (nInBuffer < bufferSize && streamW != nullptr) {
                streamWMutex.lock();
                int rd = 0;
                if (streamW != nullptr) {
                    rd = AAudioStream_read(streamW, samplesBuffer.data() + nInBuffer,
                                           bufferSize - nInBuffer, 100000000);
                }
                streamWMutex.unlock();
                if (rd <= 0) {
                    break;
                }
                nInBuffer += rd;
            }
            static int counter = 0;
            if (counter++ % 30 == 0) {
                auto ib = (float*)samplesBuffer.data();
                char buf[1000];
                sprintf(buf, "audio_main input %d: %f %f %f %f %f %f %f %f", nInBuffer, ib[0], ib[1], ib[2], ib[3], ib[4], ib[5], ib[6], ib[7]);
                logcat(buf);
            }
            memmove(microphone.writeBuf, samplesBuffer.data(), nInBuffer * 2 * sizeof(float));
            if (!microphone.swap(bufferSize)) {
                break;
            }
            nInBuffer = 0;
        }
    }

    static int logcat(const std::string &s) {
        spdlog::info("{}", s);
        android_app *app = backend::app;
        if (!app) {
            return 0;
        }
        JavaVM* java_vm = app->activity->vm;
        JNIEnv* env = NULL;
        jint jni_return = java_vm->GetEnv((void**)&env, JNI_VERSION_1_6);
        if (jni_return == JNI_ERR)
            return -1;

        jni_return = java_vm->AttachCurrentThread(&env, NULL);
        if (jni_return != JNI_OK)
            return -2;

        jclass native_activity_clazz = env->FindClass("org/sdrpp/sdrpp/AndroidAudioInput");
        if (native_activity_clazz == NULL)
            return -3;

        jmethodID method_id = env->GetStaticMethodID(native_activity_clazz, "logcat",
                                                     "([B)V");
        if (method_id == NULL)
            return -4;


        int byteCount = s.length();
        auto pNativeMessage = reinterpret_cast<const jbyte*>(s.c_str());
        auto bytes = env->NewByteArray(byteCount);
        env->SetByteArrayRegion(bytes, 0, byteCount, pNativeMessage);

        env->CallStaticVoidMethod(native_activity_clazz, method_id, bytes);

        jni_return = java_vm->DetachCurrentThread();
        if (jni_return != JNI_OK)
            return -5;
        return 0;
    }

    static void errorCallback(AAudioStream *stream, void *userData, aaudio_result_t error){
        // detect an audio device detached and restart the stream
        if (error == AAUDIO_ERROR_DISCONNECTED){
            std::thread thr(&AudioSink::restart, (AudioSink*)userData);
            thr.detach();
        }
    }

    void restart() {
        if (running) { doStop(); }
        if (running) { doStart(); }
    }

    std::thread workerThread;
    std::thread workerThreadW;

    AAudioStream *stream = NULL;
    AAudioStream *streamW = NULL;
    std::mutex streamWMutex;
    SinkManager::Stream* _stream;
    dsp::buffer::Packer<dsp::stereo_t> packer;

    std::string _streamName;
    double sampleRate;
    int bufferSize;

    bool running = false;
    bool useRawInput = false;
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
    config.setPath(core::args["root"].s() + "/android_audio_sink_config.json");
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