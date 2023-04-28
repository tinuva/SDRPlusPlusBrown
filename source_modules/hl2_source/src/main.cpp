
#ifdef _WIN32
#define _WINSOCKAPI_    // stops windows.h including winsock.h
#endif

#include <imgui.h>
#include <gui/smgui.h>
#include <utils/flog.h>
#include <module.h>
#include <gui/gui.h>
#include <signal_path/signal_path.h>
#include <core.h>
#include <config.h>

#include "hl2_device.h"
#include "utils/stream_tracker.h"

#define CONCAT(a, b) ((std::string(a) + (b)).c_str())

SDRPP_MOD_INFO {
    /* Name:            */ "hl2_source",
    /* Description:     */ "Hermes Lite 2 module for SDR++",
    /* Author:          */ "sannysanoff",
    /* Version:         */ 0, 1, 0,
    /* Max instances    */ 1
};

ConfigManager config;

//const char* AGG_MODES_STR = "Off\0Low\0High\0";

std::string discoveredToIp(DISCOVERED &d) {
    char str[INET_ADDRSTRLEN];
    inet_ntop(AF_INET, &(d.info.network.address.sin_addr), str, INET_ADDRSTRLEN);
    return str;
}

class HermesLite2SourceModule : public ModuleManager::Instance, public Transmitter {

    int adcGain = 0;
    bool sevenRelays[10]{};

public:

    explicit HermesLite2SourceModule(const std::string &name) : hermesSamples("hermes samples") {

        this->name = name;
        memset(sevenRelays, 0, sizeof(sevenRelays));

        sampleRate = 48000;

        handler.ctx = this;
        handler.selectHandler = menuSelected;
        handler.deselectHandler = menuDeselected;
        handler.menuHandler = menuHandler;
        handler.startHandler = start;
        handler.stopHandler = stop;
        handler.tuneHandler = tune;
        handler.stream = &stream;

        refresh();

        config.acquire();
        std::string devSerial = config.conf["device"];
        config.release();

        sigpath::sourceManager.registerSource("Hermes Lite 2", &handler);
        selectFirst();


    }

    ~HermesLite2SourceModule() {
        stop(this);
        sigpath::sourceManager.unregisterSource("Hermes Lite 2");
    }

    void postInit()  override  {}


    void enable()  override {
        enabled = true;
    }

    void disable() override {
        enabled = false;
    }

    bool isEnabled()  override {
        return enabled;
    }

    void refresh() {

        devices = 0;
        protocol1_discovery();

        devListTxt = "";

        for (int i = 0; i < devices; i++) {
            auto ip = discoveredToIp(discovered[i]);

            devListTxt += ip +" - " ;
            if (discovered[i].device == DEVICE_HERMES_LITE2 || discovered[i].device == DEVICE_HERMES_LITE) {
                devListTxt += std::to_string(discovered[i].supported_receivers)+" * ";
            }
            devListTxt+=discovered[i].name;

            devListTxt += '\0';
        }
    }

    void selectFirst() {
        if (devices != 0) {
            selectByIP(discoveredToIp(discovered[0]));
        }
    }

    void selectByIP(const std::string &ipAddr) {

        selectedIP = ipAddr;

        sampleRateList.clear();
        sampleRateListTxt = "";
        sampleRateList.push_back(48000);
        sampleRateList.push_back(96000);
        sampleRateList.push_back(192000);
        sampleRateList.push_back(384000);
        for(auto sr: sampleRateList) {
            sampleRateListTxt += std::to_string(sr);
            sampleRateListTxt += '\0';
        }

        selectedSerStr = ipAddr;

        // Load config here
        config.acquire();
        bool created = false;
        if (!config.conf["devices"].contains(selectedSerStr)) {
            created = true;
            config.conf["devices"][selectedSerStr]["sampleRate"] = sampleRateList[0];
        }


        // Load sample rate
        srId = 0;
//        sampleRate = sampleRateList[3];
        if (config.conf["devices"][selectedSerStr].contains("sampleRate")) {
            int selectedSr = config.conf["devices"][selectedSerStr]["sampleRate"];
            for (int i = 0; i < sampleRateList.size(); i++) {
                if (sampleRateList[i] == selectedSr) {
                    srId = i;
                    sampleRate = selectedSr;
                    break;
                }
            }
        }

        memset(sevenRelays, 0, sizeof(sevenRelays));
        if (config.conf["devices"][selectedSerStr].contains("sevenRelays")) {
            int q = config.conf["devices"][selectedSerStr]["sevenRelays"];
            if (q >=0) {
                sevenRelays[q] = true;
            }
        }

        if (config.conf["devices"][selectedSerStr].contains("adcGain")) {
            adcGain = config.conf["devices"][selectedSerStr]["adcGain"];
        }

        // Load Gains
//        if (config.conf["devices"][selectedSerStr].contains("agcMode")) {
//            agcMode = config.conf["devices"][selectedSerStr]["agcMode"];
//        }
//        if (config.conf["devices"][selectedSerStr].contains("lna")) {
//            hfLNA = config.conf["devices"][selectedSerStr]["lna"];
//        }
//        if (config.conf["devices"][selectedSerStr].contains("attenuation")) {
//            atten = config.conf["devices"][selectedSerStr]["attenuation"];
//        }

        config.release(created);

//        airspyhf_close(dev);
    }

private:

    static void menuSelected(void* ctx) {
        auto* _this = (HermesLite2SourceModule*)ctx;
        core::setInputSampleRate(_this->sampleRate);
        flog::info("HermerList2SourceModule '{0}': Menu Select!", _this->name);
    }

    static void menuDeselected(void* ctx) {
        auto* _this = (HermesLite2SourceModule*)ctx;
        flog::info("HermerList2SourceModule '{0}': Menu Deselect!", _this->name);
    }

    std::vector<dsp::complex_t> incomingBuffer;

    StreamTracker hermesSamples;

    void incomingSample(double i, double q) {
        incomingBuffer.emplace_back(dsp::complex_t{(float)q, (float)i});
        if (incomingBuffer.size() >= 512-8) {
//            hermesSamples.add(incomingBuffer.size());
            memcpy(stream.writeBuf, incomingBuffer.data(), incomingBuffer.size() * sizeof(dsp::complex_t));
            stream.swap((int)incomingBuffer.size());
            incomingBuffer.clear();

        }

    }

    static void start(void* ctx) {
        auto* _this = (HermesLite2SourceModule*)ctx;
        if (_this->running) {
            return; }
        if (_this->selectedIP.empty()) {
            flog::error("Tried to start HL2 source with null serial");
            return;
        }

        _this->device.reset();
        for(int i=0; i<devices; i++) {
            if (_this->selectedIP == discoveredToIp(discovered[i])) {
                _this->device = std::make_shared<HL2Device>(discovered[i], [=](double i, double q) {
                    static auto lastCtm = currentTimeMillis();
                    static auto totalCount = 0LL;
                    totalCount++;
                    if (totalCount % 10000 == 0) {
                        if (lastCtm < currentTimeMillis() - 1000) {
                            static auto lastLastTotalCount = 0LL;
                            auto nowCtm = currentTimeMillis();
//                            std::cout << "HL2: Speed: IQ pairs/sec: ~ " << (totalCount - lastLastTotalCount) * 1000 / (nowCtm - lastCtm) << std::endl;
                            lastLastTotalCount = totalCount;
                            lastCtm = nowCtm;
                        }
                    }
                    _this->incomingSample(i, q);
                });
            }

        }

        if (_this->device) {
            _this->device->setRxSampleRate(_this->sampleRate);
            _this->device->setADCGain(_this->adcGain);
            for(int q=0; q<6; q++) {
                if (_this->sevenRelays[q]) {
                    _this->device->setSevenRelays(1 << q);
                }
            }
            _this->device->start();
        }
        _this->running = true;
        sigpath::transmitter = _this;
        flog::info("HL2SourceModule '{0}': Start!", _this->name);
    }

    static void stop(void* ctx) {
        auto* _this = (HermesLite2SourceModule*)ctx;
        if (!_this->running) { return; }
        _this->running = false;
        _this->device->stop();
        _this->stream.stopWriter();
        _this->stream.clearWriteStop();
        flog::info("HermerList2SourceModule '{0}': Stop!", _this->name);
        _this->device.reset();

        sigpath::transmitter = nullptr;
    }

    static void tune(double freq, void* ctx) {
        auto* _this = (HermesLite2SourceModule*)ctx;
        if (_this->device) {
            _this->device->setFrequency((int)freq);
        }
        flog::info("HermerList2SourceModule '{0}': Tune: {1}!", _this->name, freq);
    }

    static void menuHandler(void* ctx) {
        auto* _this = (HermesLite2SourceModule*)ctx;

        if (_this->running) { SmGui::BeginDisabled(); }

//        SmGui::SetNextItemWidth(100);
        if (SmGui::Combo(CONCAT("##_hl2_dev_sel_", _this->name), &_this->devId, _this->devListTxt.c_str())) {
            _this->selectByIP(discoveredToIp(discovered[_this->devId]));
            core::setInputSampleRate(_this->sampleRate);
            if (!_this->selectedSerStr.empty()) {
                config.acquire();
                config.conf["device"] = _this->selectedSerStr;
                config.release(true);
            }
        }

        auto updateSampleRate = [&](int srid) {
            _this->sampleRate = (int)_this->sampleRateList[_this->srId];
            core::setInputSampleRate(_this->sampleRate);
            if (!_this->selectedSerStr.empty()) {
                config.acquire();
                config.conf["devices"][_this->selectedSerStr]["sampleRate"] = _this->sampleRate;
                config.release(true);
            }

        };
        if (SmGui::Combo(CONCAT("##_hl2_sr_sel_", _this->name), &_this->srId, _this->sampleRateListTxt.c_str())) {
            updateSampleRate(_this->srId);
        }

        SmGui::SameLine();
        SmGui::FillWidth();
        SmGui::ForceSync();
        if (SmGui::Button(CONCAT("Refresh##_hl2_refr_", _this->name))) {
            _this->refresh();
//            config.acquire();
//            std::string devSerial = config.conf["device"];
//            config.release();
            if (devices > 0) {
                _this->selectFirst();
            }
        }

        if (_this->running) { SmGui::EndDisabled(); }
        bool overload = _this->device && _this->device->isADCOverload();
        if (overload) {
            SmGui::PushStyleColor(ImGuiCol_Text, ImVec4(1.0f, 0, 0, 1.0f));
        }
        SmGui::LeftLabel("ADC Gain");
        if (overload) {
            SmGui::PopStyleColor(1);
        }
        SmGui::SameLine();
//        SmGui::SetNextItemWidth(100);
        if (SmGui::SliderInt(("##_radio_agc_gain_" + _this->name).c_str(), &_this->adcGain, -12, +48, SmGui::FMT_STR_INT_DB)) {
            if (_this->device) {
                _this->device->setADCGain(_this->adcGain);
                config.acquire();
                config.conf["devices"][_this->selectedSerStr]["adcGain"] = _this->adcGain;
                config.release(true);
            }
        }
        for(int q=0; q<6; q++) {
            char strr[100];
            switch(q) {
            case 0: sprintf(strr, "=160"); break;
            case 1: sprintf(strr, "=80"); break;
            case 2: sprintf(strr, "=40"); break;
            case 3: sprintf(strr, "=20"); break;
            case 4: sprintf(strr, "=15"); break;
            case 5: sprintf(strr, "=10"); break;
            default: sprintf(strr, "???"); break;
            }
            if (q >=0) {
                if (SmGui::RadioButton(strr, _this->sevenRelays[q])) {
                    if (_this->sevenRelays[q]) {
                        memset(_this->sevenRelays, 0, sizeof(_this->sevenRelays));
                        if (_this->device) {
                            _this->device->setSevenRelays(0);
                            config.acquire();
                            config.conf["devices"][_this->selectedSerStr]["sevenRelays"] = -1;
                            config.release(true);
                        }
                    }
                    else {
                        memset(_this->sevenRelays, 0, sizeof(_this->sevenRelays));
                        _this->sevenRelays[q] = !_this->sevenRelays[q];
                        if (_this->device) {
                            _this->device->setSevenRelays(1 << q);
                            config.acquire();
                            config.conf["devices"][_this->selectedSerStr]["sevenRelays"] = q;
                            config.release(true);
                        }
                    }
                }
            }
            if (q != 5 && q != 2) {
                SmGui::SameLine();
            }
        }

    }

    std::string name;
    bool enabled = true;
    dsp::stream<dsp::complex_t> stream = "hl2.stream";
    dsp::stream<dsp::complex_t> txstream = "hl2.txstream";
    int sampleRate;
    SourceManager::SourceHandler handler{};
    bool running = false;
    std::string selectedIP;
    int devId = 0;
    int srId = 0;
    std::string selectedSerStr;

    std::string devListTxt;
    std::vector<uint32_t> sampleRateList;
    std::string sampleRateListTxt;
    std::shared_ptr<HL2Device> device;


    int getInputStreamFramerate() override {
        return 48000;
    }
    void setTransmitStatus(bool status) override {
        device->setTune(false);
        device->setPTT(status);

    }
    void setTransmitStream(dsp::stream<dsp::complex_t>* astream) override {
        std::thread([this, astream]() {
            std::vector<dsp::complex_t> buffer;
            int addedBlocks = 0;
            int readSamples = 0;
            int nreads = 0;
            while (true) {
                int rd = astream->read();
                if (rd < 0) {
                    printf("End iq stream for tx");
                    break;
                }
                readSamples += rd;
                nreads++;
                for(int q=0; q<rd; q++) {
                    buffer.push_back(astream->readBuf[q]);
                    if (buffer.size() == 63) {
                        addedBlocks++;
                        if (addedBlocks % 1000 == 0) {
                            flog::info("Added {} blocks to tx buffer, rd samples {}  ndreads {}", addedBlocks, readSamples, nreads);
                        }
                        device->samplesToSendLock.lock();
                        device->samplesToSend.emplace_back(std::make_shared<std::vector<dsp::complex_t>>(buffer));
                        device->samplesToSendLock.unlock();
                        buffer.clear();
                    }
                }
                astream->flush();
            }
        }).detach();
    }
    void setTransmitSoftwareGain(unsigned char gain) override {
        device->setSoftwarePower(gain);

    }
    void setTransmitFrequency(int freq) override {
        device->setTxFrequency(freq);
    }

public:
    void setTransmitHardwareGain(unsigned char gain) override {
        device->setHardwarePower(gain);
    }
    unsigned char getTransmitHardwareGain() override {
        return device->hardwarePower;
    }

public:
    int getTransmittedBufferLatency() override {
        return device->bufferLatency;
    }
    void setTransmittedBufferLatency(int latency) override {
        device->bufferLatency = latency;
        device->setHangLatency(device->pttHangTime, device->bufferLatency);
    }
    int getTransmittedPttDelay() override {
        return device->pttHangTime;
    }
    void setTransmittedPttDelay(int delay) override {
        device->pttHangTime = delay;
        device->setHangLatency(device->pttHangTime, device->bufferLatency);
    }

private:
    void startGenerateTone(int frequency) override {
        device->setFrequency(frequency);
        device->setTxFrequency(frequency);
        device->setTune(true);
        device->setPTT(true);
    }

    void setPAEnabled(bool paenabled) override {
        device->setPAEnabled(paenabled);
    }

    void stopGenerateTone() override {
        device->setPTT(false);
        device->setTune(false);
    }

    void setToneGain() override {
    }

    int getTXStatus() override {
        return device->transmitMode;
    }
    float getTransmitPower() override {
        return device->fwd;
//        device->getSWR();
//        return device->fwd+device->rev;
    }

public:
    float getReflectedPower() override {
        return device->rev;
    }

private:
    float getTransmitSWR() override {
        return device->swr;
    }

    float getFillLevel() override {
        return (float)device->fill_level;
    }

    std::string& getTransmitterName() override {
        static std::string name = "Hermes Lite 2";
        return name;
    }
};

MOD_EXPORT void _INIT_() {
#ifdef WIN32
    int iResult;

// Initialize Winsock
    WSADATA wsaData;

    iResult = WSAStartup(MAKEWORD(2,2), &wsaData);
    if (iResult != 0) {
        flog::error("WSAStartup failed: %d\n", iResult);
        exit(1);
    }
#endif
    json def = json({});
    def["devices"] = json({});
    def["device"] = "";
    config.setPath(core::args["root"].s() + "/hl2_config.json");
    config.load(def);
    config.enableAutoSave();
}

MOD_EXPORT ModuleManager::Instance* _CREATE_INSTANCE_(std::string name) {
    return new HermesLite2SourceModule(name);
}

MOD_EXPORT void _DELETE_INSTANCE_(ModuleManager::Instance* instance) {
    delete (HermesLite2SourceModule*)instance;
}

MOD_EXPORT void _END_() {
    config.disableAutoSave();
    config.save();
}