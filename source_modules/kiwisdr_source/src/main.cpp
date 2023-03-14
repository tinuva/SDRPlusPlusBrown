#ifdef _WIN32
#include <winsock2.h>
#include <ws2ipdef.h>
#include <ws2tcpip.h>
inline void usleep(int micros) {
    Sleep(micros / 1000);
}
#endif

#include <imgui.h>
#include <utils/flog.h>
#include <module.h>
#include <gui/gui.h>
#include <signal_path/signal_path.h>
#include <core.h>
#include <gui/style.h>
#include <config.h>
#include <gui/widgets/stepped_slider.h>
#include <utils/optionlist.h>
#include <gui/dialogs/dialog_box.h>
#include "utils/proto/websock.h"


SDRPP_MOD_INFO{
    /* Name:            */ "kiwisdr_source",
    /* Description:     */ "KiwiSDR WebSDR source module for SDR++",
    /* Author:          */ "san",
    /* Version:         */ 0, 1, 0,
    /* Max instances    */ 1
};

struct KiwiSDRClient {
    std::string hostPort;

};

ConfigManager config;

struct KiwiSDRSourceModule : public ModuleManager::Instance {

    net::websock::WSClient wsClient;
    std::vector<std::complex<float>> iqData;
    std::mutex iqDataLock;

    static constexpr int IQDATA_FREQUENCY = 12000;
    static constexpr int NETWORK_BUFFER_SECONDS = 2;
    static constexpr int NETWORK_BUFFER_SIZE = NETWORK_BUFFER_SECONDS * IQDATA_FREQUENCY;
    std::string kiwisdrSite = "kiwi-iva.aprs.fi";
    char connectionStatus[100];
    std::vector<int64_t> times;
    int64_t lastPing;

    KiwiSDRSourceModule(std::string name) : wsClient(), iqDataLock(), iqData() {
        this->name = name;
        strcpy(connectionStatus,"Not connected");

        // Yeah no server-ception, sorry...
        // Initialize lists
        handler.ctx = this;
        handler.selectHandler = menuSelected;
        handler.deselectHandler = menuDeselected;
        handler.menuHandler = menuHandler;
        handler.startHandler = start;
        handler.stopHandler = stop;
        handler.tuneHandler = tune;
        handler.stream = &stream;


        wsClient.onDisconnected = [&]() {
            connected = false;
        };

        wsClient.onConnected = [&]() {
            //x.sendString("SET mod=usb low_cut=300 high_cut=2700 freq=14100.000");
            wsClient.sendString("SET auth t=kiwi p=#");
            wsClient.sendString("SET AR OK in=12000 out=48000");
            //            x.sendString("SET mod=am low_cut=-4900 high_cut=4900 freq=119604.33");
            wsClient.sendString("SERVER DE CLIENT sdr++brown SND");
            wsClient.sendString("SET compression=0");
            wsClient.sendString("SET agc=0 hang=0 thresh=-100 slope=6 decay=1000 manGain=50");
            connected = true;
            tune(lastTuneFrequency, this);
            strcpy(connectionStatus, "Connected, waiting data...");
//            wsClient.sendString("SET mod=iq low_cut=-5000 high_cut=5000 freq=14074.000");

        };
        wsClient.onTextMessage = [&](const std::string &msg) {
            flog::info("TEXT: {}", msg);
        };

        wsClient.onBinaryMessage = [&](const std::string &msg) {
            std::string start = "???";
            if (msg.size() > 3) {
                start = msg.substr(0, 3);
            }
            if (start == "MSG") {
                flog::info("BIN/MSG: {} text: {}", msg.size(), msg);
            } else if (start == "SND") {
                flog::info("{} Got sound: bytes={} )", (int64_t)currentTimeMillis(), (int64_t)msg.size());
                long long int ctm = currentTimeMillis();
                times.emplace_back(ctm);
                int lastSecondCount = 0;
                for(int q=times.size()-1; q>=0; q--) {
                    if (times[q] < ctm - 1000) {
                        break;
                    }
                    lastSecondCount++;
                }
                while(!times.empty() && times.front() < ctm - 2000) {
                    times.erase(times.begin());
                }
                sprintf(connectionStatus, "Receiving. %d KB/sec (%d)", (lastSecondCount*((int)msg.size()))/1024, lastSecondCount);
                int HEADER_SIZE = 20;
                if (msg[3] == 0x08 && msg.size() == 2048 + HEADER_SIZE) { // IQ data
                    auto scan = msg.data();
                    scan += 4;
                    auto sequence = *(int32_t *)scan; scan += 4;
                    char one = *(char *)scan; scan += 1;
                    auto info1 = *(int16_t *)scan; scan += 2;
                    char zero = *(char *)scan; scan += 1;
                    auto timestamp = *(int32_t *)scan; scan += 4;
                    auto *arg2 = (int32_t *)scan; scan += 4;

                    if (scan != msg.data() + 20) {
                        abort();        // homegrown assert.
                    }

                    int16_t *ptr = (int16_t *)scan;
                    int buflen = 0;
                    iqDataLock.lock();
                    for(int z=0; z<512; z++) {
                        int16_t *iqsample = &ptr[2*z];
                        char *fourbytes = (char *)iqsample;
                        std::swap(fourbytes[0], fourbytes[1]);
                        std::swap(fourbytes[2], fourbytes[3]);
                        iqData.emplace_back(iqsample[0]/32767.0, iqsample[1]/32767.0);
                    }
                    int erased = 0;
                    while(iqData.size() > NETWORK_BUFFER_SIZE * 1.5) {
                        iqData.erase(iqData.begin(), iqData.begin()+200);
                        erased += 200;
                    }
                    buflen = iqData.size();
                    iqDataLock.unlock();
//                    flog::info("{} Got sound: bytes={} sequence={} info1={} timestamp={} , {} samples, buflen now = {} (erased {})", (int64_t)currentTimeMillis(), msg.size(), sequence, info1, timestamp, (msg.size()-HEADER_SIZE) / 4, buflen, erased);
                }
            } else {
                if (msg.size() >= 70) {
                    char buf[100];
                    for (int q = 3; q < 30; q++) {
                        sprintf(buf, "%02x ", (unsigned char)msg[q]);
                        start += buf;
                    }
                    start += "... ";
                    for (int q = -20; q < 0; q++) {
                        sprintf(buf, "%02x ", (unsigned char)msg[msg.size()-1+q]);
                        start += buf;
                    }
                }
                flog::info("BIN: {} bytes: {}", msg.size(), start);
            }
        };
        lastPing = currentTimeMillis();
        wsClient.onEveryReceive = [&]() {
            if (currentTimeMillis() - lastPing > 4000) {
                wsClient.sendString("SET keepalive");
                lastPing = currentTimeMillis();
            }
        };


        // Load config

        sigpath::sourceManager.registerSource("KiwiSDR", &handler);
    }

    ~KiwiSDRSourceModule() {
        stop(this);
        sigpath::sourceManager.unregisterSource("KiwiSDR");
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

    static void menuSelected(void* ctx) {
        KiwiSDRSourceModule* _this = (KiwiSDRSourceModule*)ctx;
        core::setInputSampleRate(12000); // fixed for kiwisdr
        flog::info("KiwiSDRSourceModule '{0}': Menu Select!", _this->name);
    }

    static void menuDeselected(void* ctx) {
        KiwiSDRSourceModule* _this = (KiwiSDRSourceModule*)ctx;
        flog::info("KiwiSDRSourceModule '{0}': Menu Deselect!", _this->name);
    }

    static void start(void* ctx) {
        KiwiSDRSourceModule* _this = (KiwiSDRSourceModule*)ctx;
        if (_this->running) { return; }
        strcpy(_this->connectionStatus,"Connecting..");
        sigpath::iqFrontEnd.setCurrentStreamTime(0); // not started
        _this->nextSend = 0;
        _this->running = true;
        _this->timeSet = false;
        std::thread looper([=]() {
            flog::info("calling x.connectAndReceiveLoop..");
            try {
                _this->wsClient.connectAndReceiveLoop(_this->kiwisdrSite, 8073, "/kiwi/" + std::to_string(currentTimeMillis()) + "/SND");
                flog::info("x.connectAndReceiveLoop exited.");
                strcpy(_this->connectionStatus, "Disconnected");
            } catch (const std::runtime_error& e) {
                flog::error("KiwiSDRSourceModule: Exception: {}", e.what());
                strcpy(_this->connectionStatus,"Error: ");
                strcat(_this->connectionStatus, e.what());
                _this->running = false;
            }
        });
        std::thread feeder([=]() {
            double nextSend = 0;
            while(_this-> running){
                _this->iqDataLock.lock();
                auto bufsize = _this->iqData.size();
                _this->iqDataLock.unlock();
                double now = (double)currentTimeMillis();
                if (nextSend == 0) {
                    if (bufsize < 200) {
                        usleep(16000); // some sleep
                        continue ; // waiting for initial batch
                    }
                    nextSend = now;
                } else {
                    auto delay = nextSend - now;
                    double sleepTime = delay * 1000;
                    if (sleepTime > 0) {
                        usleep(sleepTime);
                    }
                }
                std::vector<std::complex<float>> toSend;
                int bufferSize = 0;
                _this->iqDataLock.lock();
                if (_this->iqData.size() >= 200) {
                    for(int i=0; i<200; i++) {
                        toSend.emplace_back(_this->iqData[i]);
                    }
                    _this->iqData.erase(_this->iqData.begin(), _this->iqData.begin()+200);
                    bufferSize = _this->iqData.size();
                }
                _this->iqDataLock.unlock();
                if (bufferSize > NETWORK_BUFFER_SIZE) {
                    nextSend += 1000.0 / 120.0;
                } else {
                    nextSend += 1000.0 / 60.0;
                }
                int64_t ctm = currentTimeMillis();
                if (!toSend.empty()) {
//                    flog::info("{} Sending samples! buf remain = {}", ctm, bufferSize);
                    memcpy(_this->stream.writeBuf, toSend.data(), toSend.size() * sizeof(dsp::complex_t));
                    _this->stream.swap((int)toSend.size());
                } else {
                    nextSend = 0;
//                    flog::info("{} Underflow of KiwiSDR iq data!", ctm);
                }
                long long int newStreamTime = currentTimeMillis() - (bufferSize / IQDATA_FREQUENCY) - 500; // just 500.
                if (!_this->timeSet) {
                    sigpath::iqFrontEnd.setCurrentStreamTime(newStreamTime);
                    _this->timeSet = true;
                } else {
                    if (sigpath::iqFrontEnd.getCurrentStreamTime() < newStreamTime) {
                        sigpath::iqFrontEnd.setCurrentStreamTime(newStreamTime);
                    }
                }
            }

        });
        feeder.detach();
        looper.detach();

        _this->running = true;
        flog::info("KiwiSDRSourceModule '{0}': Start!", _this->name);
    }

    static void stop(void* ctx) {
        KiwiSDRSourceModule* _this = (KiwiSDRSourceModule*)ctx;
        if (!_this->running) { return; }
        strcpy(_this->connectionStatus,"Disconnecting..");

        _this->wsClient.stopSocket();

        _this->running = false;
        flog::info("KiwiSDRSourceModule '{0}': Stop!", _this->name);
    }

    std::vector<dsp::complex_t> incomingBuffer;

    double nextSend = 0;

    void incomingSample(double i, double q) {
        incomingBuffer.emplace_back(dsp::complex_t{(float)q, (float)i});
        if (incomingBuffer.size() >= 200) {     // 60 times per second
            double now = (double)currentTimeMillis();
            if (nextSend == 0) {
                nextSend = now;
            } else {
                auto delay = nextSend - now;
                double sleepTime = delay * 1000;
                if (sleepTime > 0) {
                    usleep(sleepTime);
                }
            }
//            flog::info("Sending samples: {}", incomingBuffer.size());
            nextSend += 1000.0 / 60.0;
            incomingBuffer.clear();
        }

    }


    double lastTuneFrequency = 14.100;

    static void tune(double freq, void* ctx) {
        KiwiSDRSourceModule* _this = (KiwiSDRSourceModule*)ctx;
//        if (_this->running && _this->client) {
//            _this->client->setFrequency(freq);
//        }
//        _this->freq = freq;
        _this->lastTuneFrequency = freq;
        if (_this->running && _this->connected) {
            char buf[1024];
            sprintf(buf, "SET mod=iq low_cut=-7000 high_cut=7000 freq=%0.3f", freq/ 1000.0);
            _this->wsClient.sendString(buf);
        }
        flog::info("KiwiSDRSourceModule '{0}': Tune: {1}!", _this->name, freq);
    }

    static void menuHandler(void* ctx) {
        KiwiSDRSourceModule* _this = (KiwiSDRSourceModule*)ctx;

//
//        if (_this->fileSelect.render("##file_source_" + _this->name)) {
//            if (_this->fileSelect.pathIsValid()) {
//                if (_this->reader != NULL) {
//                    _this->reader->close();
//                    delete _this->reader;
//                }
//                try {
//                    _this->reader = new WavReader(_this->fileSelect.path);
//                    _this->sampleRate = _this->reader->getSampleRate();
//                    core::setInputSampleRate(_this->sampleRate);
//                    std::string filename = std::filesystem::path(_this->fileSelect.path).filename().string();
//                    double newFrequency = _this->getFrequency(filename);
//                    _this->streamStartTime = _this->getStartTime(filename);
//                    bool fineTune = gui::waterfall.containsFrequency(newFrequency);
//                    //                    auto prevFrequency = sigpath::vfoManager.getName();
//                    _this->centerFreq = newFrequency;
//                    _this->centerFreqSet = true;
//                    tuner::tune(tuner::TUNER_MODE_IQ_ONLY, "", _this->centerFreq);
//                    if (fineTune) {
//                        // restore the fine tune. When working with file source and restarting the app, the fine tune is lost
//                        //                        tuner::tune(tuner::TUNER_MODE_NORMAL, "_current", prevFrequency);
//                    }
//                    //gui::freqSelect.minFreq = _this->centerFreq - (_this->sampleRate/2);
//                    //gui::freqSelect.maxFreq = _this->centerFreq + (_this->sampleRate/2);
//                    //gui::freqSelect.limitFreq = true;
//                }
//                catch (std::exception e) {
//                    flog::error("Error: {0}", e.what());
//                }
//                config.acquire();
//                config.conf["path"] = _this->fileSelect.path;
//                config.release(true);
//            }
//        }

        ImGui::Text("KiwiSDR site: %s", _this->kiwisdrSite.c_str());
        ImGui::Text("Status: %s", _this->connectionStatus);

        long long int cst = sigpath::iqFrontEnd.getCurrentStreamTime();
        std::time_t t = cst /1000;
        auto tmm = std::localtime(&t);
        char streamTime[64];
        strftime(streamTime, sizeof(streamTime), "%Y-%m-%d %H:%M:%S", tmm);
        ImGui::Text("Stream pos: %s", streamTime);


    }


    std::string name;
    bool enabled = true;
    bool running = false;
    bool connected = false;
    bool timeSet = false;

    double freq;
    bool serverBusy = false;

    dsp::stream<dsp::complex_t> stream;
    SourceManager::SourceHandler handler;

    std::shared_ptr<KiwiSDRClient> client;
};

MOD_EXPORT void _INIT_() {
    json def = json({});
    config.setPath(core::args["root"].s() + "/kiwisdr_source_config.json");
    config.load(def);
    config.enableAutoSave();
}

MOD_EXPORT ModuleManager::Instance* _CREATE_INSTANCE_(std::string name) {
    return new KiwiSDRSourceModule(name);
}

MOD_EXPORT void _DELETE_INSTANCE_(ModuleManager::Instance* instance) {
    delete (KiwiSDRSourceModule*)instance;
}

MOD_EXPORT void _END_() {
    config.disableAutoSave();
    config.save();
}