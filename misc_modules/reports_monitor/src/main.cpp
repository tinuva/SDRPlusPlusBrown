
#include "gui/brown/kiwisdr_map.h"
#include "gui/brown/small_waterfall.h"

#include <imgui.h>
#include <module.h>
#include <gui/gui.h>
#include "utils/proto/reporter_services.h"
#include <core.h>
#include <signal_path/signal_path.h>
#include <config.h>
#include <ctm.h>
#include <gui/widgets/waterfall.h>
#include "../../decoder_modules/ft8_decoder/src/module_interface.h"
#include "utils/wav.h"
#include "gui/brown/imgui-notify/imgui_notify.h"

#define MAX_COMMAND_LENGTH 8192

SDRPP_MOD_INFO{
        /* Name:            */ "reports_monitor",
        /* Description:     */ "PSKreporter, WSPRnet, RBN",
        /* Author:          */ "San",
        /* Version:         */ 0, 1, 0,
        /* Max instances    */ -1
};

std::string convertToMorseCode(std::string input) {
    // Define Morse Code map
    std::unordered_map<char, std::string> morse {
            {'A', ".-"}, {'B', "-..."}, {'C', "-.-."}, {'D', "-.."},
            {'E', "."}, {'F', "..-."}, {'G', "--."}, {'H', "...."},
            {'I', ".."}, {'J', ".---"}, {'K', "-.-"}, {'L', ".-.."},
            {'M', "--"}, {'N', "-."}, {'O', "---"}, {'P', ".--."},
            {'Q', "--.-"}, {'R', ".-."}, {'S', "..."}, {'T', "-"},
            {'U', "..-"}, {'V', "...-"}, {'W', ".--"}, {'X', "-..-"},
            {'Y', "-.--"}, {'Z', "--.."},

            {'1', ".----"}, {'2', "..---"}, {'3', "...--"}, {'4', "....-"},
            {'5', "....."}, {'6', "-...."}, {'7', "--..."}, {'8', "---.."},
            {'9', "----."}, {'0', "-----"},

            {',', "--..--"},  // Comma
            {'.', ".-.-.-"},  // Period
            {'?', "..--.."},  // Question mark
            {'!', "-.-.--"},  // Exclamation mark
            {'-', "-....-"},  // Hyphen
            {'/', "-..-."},   // Slash
            {'@', ".--.-."},  // At symbol
            {'(', "-.--."},   // Open parenthesis
            {')', "-.--.-"},  // Close parenthesis
            {'\'', ".----."},  // Apostrophe
            {'\"', ".-..-."},  // Quotation mark
            {':', "---..."},  // Colon
            {';', "-.-.-."},  // Semicolon
            {'=', "-...-"},  // Equal sign
            {'+', ".-.-."},  // Plus sign
            {'_', "..--.-"},  // Underscore
            {'$', "...-..-"},  // Dollar sign
            {'&', ".-..."}  // Ampersand
    };

    std::string output;
    for (const auto &ch : input) {
        if (ch == ' ') {
            output += "   ";  // Three spaces for word separator
        } else {
            char upperChar = toupper(ch);  // Convert to uppercase
            if(morse.count(upperChar) > 0) {  // If the character is in the map
                output += morse[upperChar];
                output += " ";  // One space for letter separator
            }
        }
    }
    
    return output;
}


std::vector<dsp::stereo_t> generateMorseAudio(int sampleRate, int freq, int ditLengthMs, const std::string &morse) {
    std::vector<dsp::stereo_t> samples;
    const float ditDuration = ditLengthMs / 1000.0f;
    const float envelopeDuration = ditDuration / 20.0f;
    const int ditSamples = static_cast<int>(ditDuration * sampleRate);
    const int envelopeSamples = static_cast<int>(envelopeDuration * sampleRate);
    const float pi = std::acos(-1);
    
    for (char c : morse) {
        if (c == '.' || c == '-') {
            int totalSamples = (c == '.' ? ditSamples : 3 * ditSamples);
            for (int i = 0; i < totalSamples; i++) {
                float envelope = 1.0f;
                if (i < envelopeSamples) {
                    envelope = static_cast<float>(i) / envelopeSamples;
                } else if (i >= totalSamples - envelopeSamples) {
                    envelope = static_cast<float>(totalSamples - i) / envelopeSamples;
                }
                float sample = envelope * sin(2.0f * pi * freq * i / sampleRate);
                samples.push_back(dsp::stereo_t{sample, sample});
            }
            for (int i = 0; i < ditSamples; i++) {
                samples.push_back(dsp::stereo_t{0, 0});
            }
        } else if (c == ' ') {
            for (int i = 0; i < 3 * ditSamples; i++) {
                samples.push_back(dsp::stereo_t{0, 0});
            }
        }
    }
    auto halfBandPassTaps = dsp::taps::lowPass0<dsp::complex_t>(800, 100, trxAudioSampleRate);
    dsp::filter::FIR<dsp::stereo_t, dsp::complex_t> lpf(nullptr, halfBandPassTaps);
    lpf.process(samples.size(), samples.data(), samples.data());
    return samples;
}

ConfigManager config;

class ReportsMonitorModule;

ReportsMonitorModule *modul;

class ReportsMonitorModule : public ModuleManager::Instance {

public:

    std::string monitoringCallsign;
    static ReportsMonitorModule *instance;

    ReportsMonitorModule(std::string name, std::string root) {
        this->name = name;
        modul = this;
        gui::menu.registerEntry(name, _menuHandler, this, NULL);
    }

    ~ReportsMonitorModule() {
        gui::menu.removeEntry(name);
    }

    void addReport(const net::Report &report) {
        reportsMutex.lock();
        bool found = false;
        for(int q=0; q<reports.size(); q++) {
            auto &rep = reports[q];
            if (rep.mode == report.mode && rep.reporterCallsign == report.reporterCallsign && rep.timestamp == report.timestamp) {
                found = true;
                break;
            }
        }
        if (!found) {
            auto nr = report;
            nr.timestamp = currentTimeMillis();
            reports.emplace_back(nr);
        }
        reportsMutex.unlock();

    }

    struct ReportingService {
        net::ReportingSource source;
        bool running = false;
        long long started = 0;
        std::shared_ptr<std::thread> thr;
        std::string status = "not monitoring";
        bool stopping = false;
        std::mutex mtx;
        int displayCount;

        auto callbackReceiver() {
            return [&](const ::net::Report &report) {
                if (!running) {
                    return;
                }
                if (report.errorStatus != "") {
                    setStatus(report.errorStatus);
                    return;
                }
                modul->addReport(report);
            };
        }

        void toggleWithButton() {
            if (stopping) {
                // do nothing
            } else {
                if (running) {
                    stop();
                } else {
                    start();
                }
            }
        }

        void setStatus(std::string status) {
            mtx.lock();
            this->status = status;
            mtx.unlock();
        }

        void stop() {
            if (!stopping) {
                if (running) {
                    setStatus("Stopping.");
                    stopping = true;
                }
                running = false;
                started = 0;
            }
        }

        void start() {
            if (!running) {
                running = true;
                started = currentTimeMillis();
                if (!thr) {
                    thr = std::make_shared<std::thread>([&]() {
                        while (running) {
                            switch(source) {
                                case net::RS_WSPRNET:
                                    net::getReportsFromWSPR(sigpath::iqFrontEnd.operatorCallsign, callbackReceiver(), running);
                                    break;
                                case net::RS_PSKREPORTER:
                                    net::getReportsFromPSKR(sigpath::iqFrontEnd.operatorCallsign, callbackReceiver(), running);
                                    break;
                                case net::RS_RBN:
                                    net::getReportsFromRBN(sigpath::iqFrontEnd.operatorCallsign, callbackReceiver(), running);
                                    break;
                            }
                        }
                        thr->detach();
                        thr.reset();
                        stopping = false;
                        setStatus("Stopped.");
                    });
                }
            }
        }

        void maintainThread(int receiveDuration) {
            long long int ctm = currentTimeMillis();
            if (started != 0 && ctm - currentTimeMillis() > receiveDuration * 1000) {
                stop();
            }
        }

    };

    int receiveDuration = 180;       // seconds
    int aggregateDuration = 600;
    bool visible = true;

    std::vector<net::Report> reports;
    std::mutex reportsMutex;

    ReportingService wspr;
    ReportingService rbn;
    ReportingService pskr;

    void postInit() {
        config.acquire();
        if (config.conf.contains("receiveDuration")) {
            receiveDuration = config.conf["receiveDuration"];
        }
        if (config.conf.contains("aggregateDuration")) {
            aggregateDuration = config.conf["aggregateDuration"];
        }
        config.release(false);
        rbn.source = net::RS_RBN;
        pskr.source = net::RS_PSKREPORTER;
        wspr.source = net::RS_WSPRNET;
    }

    void enable() {
        enabled = true;
    }

    void disable() {
        enabled = false;
        rbn.stop();
        pskr.stop();
        wspr.stop();
    }

    bool isEnabled() {
        return enabled;
    }

    void maintainThreads() {
        pskr.maintainThread(receiveDuration);
        rbn.maintainThread(receiveDuration);
        wspr.maintainThread(receiveDuration);
    }




private:
    void menuHandler() {
        maintainThreads();
        ImGui::Text("Operator Callsign: %s", sigpath::iqFrontEnd.operatorCallsign.c_str());

        ImGui::LeftLabel("Receive:");
        ImGui::SameLine();
        if (ImGui::SliderInt("##_websdr_recvduration_", &receiveDuration, 15, 600, "%d sec")) {
            config.acquire();
            config.conf["receiveDuration"] = receiveDuration;
            config.release(true);

        }
        ImGui::LeftLabel("Aggregate:");
        ImGui::SameLine();
        if (ImGui::SliderInt("##_websdr_aggduration_", &aggregateDuration, 15, 600, "%d sec")) {
            config.acquire();
            config.conf["aggregateDuration"] = aggregateDuration;
            config.release(true);

        }
        ImGui::LeftLabel("Enabled");
        if (ImGui::Checkbox("##_websdr_visible_", &visible)) {
            config.acquire();
            config.conf["visible"] = visible;
            config.release(true);
            reports.clear();
        }
        ImGui::BeginDisabled(sigpath::iqFrontEnd.operatorCallsign.empty());
        if (doFingerButton("WSPR")) {
            wspr.toggleWithButton();
        }
        ImGui::SameLine();
        if (doFingerButton("RBN(CW)")) {
            rbn.toggleWithButton();
        }
        ImGui::SameLine();
        if (doFingerButton("PSKR(FT8)")) {
            pskr.toggleWithButton();
        }
        if (reports.size() > 0) {
            if (doFingerButton("View..")) {
            }
        }
        ImGui::EndDisabled();
        wspr.displayCount = 0;
        rbn.displayCount = 0;
        pskr.displayCount = 0;
        reportsMutex.lock();
        long long int ctm = currentTimeMillis();
        for (auto &r:  reports) {
            if (ctm - r.createdTimestamp > aggregateDuration * 1000) {
                continue;
            }
            switch(r.reportingSource) {
                case net::RS_RBN:
                    rbn.displayCount++;
                    break;
                case net::RS_WSPRNET:
                    wspr.displayCount++;
                    break;
                case net::RS_PSKREPORTER:
                    pskr.displayCount++;
                    break;
            }
        }
        reportsMutex.unlock();
        ImGui::Text("WSPR (%d): %s", wspr.displayCount, wspr.status.c_str());
        ImGui::Text("RBN (%d): %s", rbn.displayCount, rbn.status.c_str());
        ImGui::Text("PSKR (%d): %s", pskr.displayCount, pskr.status.c_str());

        auto disabled = sigpath::iqFrontEnd.operatorCallsign.empty() || !sigpath::transmitter || sigpath::transmitter->getTXStatus() || !gui::mainWindow.canTransmit();
        ImGui::BeginDisabled(disabled);
        char buf[1024];
        sprintf(buf, "CW TX: CQ CQ DE %s %s K", sigpath::iqFrontEnd.operatorCallsign.c_str(), sigpath::iqFrontEnd.operatorCallsign.c_str());
        if (doFingerButton(buf)) {
            transmitCQ();
        }

        FT8ModuleInterface *ft8 = nullptr;
        for(auto x: core::moduleManager.instances) {
            ft8 = dynamic_cast<FT8ModuleInterface *>(x.second.instance);
            if (ft8) {
                break;
            }
        }
        if (ft8) {
            if (doFingerButton("WSPR TX: 5W power")) {
                transmitWSPR();
            }
            sprintf(buf, "FT8 TX: CQ %s %s", sigpath::iqFrontEnd.operatorCallsign.c_str(), sigpath::iqFrontEnd.operatorLocation.substr(0, 4).c_str());
            if (doFingerButton(buf)) {
                transmitFT8();
            }
        } else {
            ImGui::Text("Load FT8 module to tx FT8/WSPR");
        }
        ImGui::EndDisabled();
    }

    std::shared_ptr<std::vector<dsp::stereo_t>> saudio = std::make_shared<std::vector<dsp::stereo_t>>();

    void transmitCQ() {
        char buf[1024];
        sprintf(buf, "CQ CQ DE %s %s K", sigpath::iqFrontEnd.operatorCallsign.c_str(), sigpath::iqFrontEnd.operatorCallsign.c_str());
        const std::string morse = " " +convertToMorseCode(buf)+ " " ;

        auto ditDuration = 60 * 1000 / (50 * gui::mainWindow.cwWPM);

        auto audio = generateMorseAudio(trxAudioSampleRate, gui::mainWindow.cwAudioFrequency, ditDuration, morse);
        saudio->clear();
        saudio->insert(saudio->end(), audio.begin(), audio.end());

//        wav::Writer w;
//        w.setChannels(2);
//        w.setFormat(wav::FORMAT_WAV);
//        w.setSampleType(wav::SAMP_TYPE_FLOAT32);
//        w.setSamplerate(trxAudioSampleRate);
//        if (!w.open("/tmp/t1.wav")) {
//            ImGui::InsertNotification({ ImGuiToastType_Error, 5000, ("Write err: " + std::string(strerror(errno))).c_str() });
//            return;
//        }
//        w.write((float *)saudio->data(), saudio->size());
//        w.close();

        auto saved = gui::mainWindow.getCurrentModeAttr("submode");

        gui::mainWindow.maybeTransmit(saudio,
                                      [=]() { gui::mainWindow.setCurrentModeBySubmode("CW"); },
                                      [=]() { gui::mainWindow.setCurrentModeBySubmode(saved); });
        flog::info("Done {}", morse);
    }
    
    void transmitWSPR() {

    }

    void transmitFT8() {

    }

    static void _menuHandler(void *ctx) {
        ((ReportsMonitorModule *) ctx)->menuHandler();
    }

    std::string name;
    bool enabled = true;
};


MOD_EXPORT void _INIT_() {
    config.setPath(core::args["root"].s() + "/repors_monitor.json");
    config.load(json::object());
    config.enableAutoSave();
}

MOD_EXPORT ModuleManager::Instance *_CREATE_INSTANCE_(std::string name) {
    return new ReportsMonitorModule(name, core::args["root"].s());
}

MOD_EXPORT void _DELETE_INSTANCE_(void *instance) {
    delete (ReportsMonitorModule *) instance;
}

MOD_EXPORT void _END_() {
    config.disableAutoSave();
    config.save();
}

