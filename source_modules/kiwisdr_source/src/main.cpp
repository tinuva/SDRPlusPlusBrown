#define IMGUI_DEFINE_MATH_OPERATORS
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
#include <gui/widgets/finger_button.h>
#include <signal_path/signal_path.h>
#include <core.h>
#include <gui/widgets/geomap.h>
#include <gui/gui.h>
#include <config.h>
#include <utils/optionlist.h>
#include "utils/proto/websock.h"
#include "utils/usleep.h"
#include <filesystem>
#include <chrono>
#include <fstream>


SDRPP_MOD_INFO{
    /* Name:            */ "kiwisdr_source",
    /* Description:     */ "KiwiSDR WebSDR source module for SDR++",
    /* Author:          */ "san",
    /* Version:         */ 0, 1, 0,
    /* Max instances    */ 1
};

struct KiwiSDRClient {
    net::websock::WSClient wsClient;
    std::string hostPort;
    bool connected;
    char connectionStatus[100];
    int64_t lastPing;
    bool running;
    std::vector<int64_t> times;

    std::function<void()> onConnected = [](){};
    std::function<void()> onDisconnected = [](){};
    std::vector<std::complex<float>> iqData;
    std::mutex iqDataLock;
    static constexpr int IQDATA_FREQUENCY = 12000;
    static constexpr int NETWORK_BUFFER_SECONDS = 2;
    static constexpr int NETWORK_BUFFER_SIZE = NETWORK_BUFFER_SECONDS * IQDATA_FREQUENCY;



    void init(const std::string &hostport, int lastTuneFrequency) {

        this->hostPort = hostport;
        strcpy(connectionStatus, "Not connected");

        wsClient.onDisconnected = [&]() {
            connected = false;
        };

        wsClient.onConnected = [&]() {
            // x.sendString("SET mod=usb low_cut=300 high_cut=2700 freq=14100.000");
            wsClient.sendString("SET auth t=kiwi p=#");
            wsClient.sendString("SET AR OK in=12000 out=48000");
            //            x.sendString("SET mod=am low_cut=-4900 high_cut=4900 freq=119604.33");
            wsClient.sendString("SERVER DE CLIENT sdr++brown SND");
            wsClient.sendString("SET compression=0");
            wsClient.sendString("SET agc=0 hang=0 thresh=-100 slope=6 decay=1000 manGain=50");
            connected = true;
            if (this->onConnected) {
                onConnected();
            }
            strcpy(connectionStatus, "Connected, waiting data...");
            //            wsClient.sendString("SET mod=iq low_cut=-5000 high_cut=5000 freq=14074.000");
        };
        wsClient.onTextMessage = [&](const std::string& msg) {
            flog::info("TEXT: {}", msg);
        };

        wsClient.onBinaryMessage = [&](const std::string& msg) {
            std::string start = "???";
            if (msg.size() > 3) {
                start = msg.substr(0, 3);
            }
            if (start == "MSG") {
                flog::info("BIN/MSG: {} text: {}", (int64_t)msg.size(), msg);
            }
            else if (start == "SND") {
                //                flog::info("{} Got sound: bytes={} )", (int64_t)currentTimeMillis(), (int64_t)msg.size());
                long long int ctm = currentTimeMillis();
                times.emplace_back(ctm);
                int lastSecondCount = 0;
                for (int q = times.size() - 1; q >= 0; q--) {
                    if (times[q] < ctm - 1000) {
                        break;
                    }
                    lastSecondCount++;
                }
                while (!times.empty() && times.front() < ctm - 2000) {
                    times.erase(times.begin());
                }
                sprintf(connectionStatus, "Receiving. %d KB/sec (%d)", (lastSecondCount * ((int)msg.size())) / 1024, lastSecondCount);
                int HEADER_SIZE = 20;
                if (msg[3] == 0x08 && msg.size() == 2048 + HEADER_SIZE) { // IQ data
                    auto scan = msg.data();
                    scan += 4;
                    auto sequence = *(int32_t*)scan;
                    scan += 4;
                    char one = *(char*)scan;
                    scan += 1;
                    auto info1 = *(int16_t*)scan;
                    scan += 2;
                    char zero = *(char*)scan;
                    scan += 1;
                    auto timestamp = *(int32_t*)scan;
                    scan += 4;
                    auto* arg2 = (int32_t*)scan;
                    scan += 4;

                    if (scan != msg.data() + 20) {
                        abort(); // homegrown assert.
                    }

                    int16_t* ptr = (int16_t*)scan;
                    int buflen = 0;
                    iqDataLock.lock();
                    for (int z = 0; z < 512; z++) {
                        int16_t* iqsample = &ptr[2 * z];
                        char* fourbytes = (char*)iqsample;
                        std::swap(fourbytes[0], fourbytes[1]);
                        std::swap(fourbytes[2], fourbytes[3]);
                        iqData.emplace_back(iqsample[0] / 32767.0, iqsample[1] / 32767.0);
                    }
                    int erased = 0;
                    while (iqData.size() > NETWORK_BUFFER_SIZE * 1.5) {
                        iqData.erase(iqData.begin(), iqData.begin() + 200);
                        erased += 200;
                    }
                    buflen = iqData.size();
                    iqDataLock.unlock();
//                    flog::info("{} Got sound: bytes={} sequence={} info1={} timestamp={} , {} samples, buflen now = {} (erased {})", (int64_t)currentTimeMillis(), msg.size(), sequence, info1, timestamp, (msg.size() - HEADER_SIZE) / 4, buflen, erased);
                }
            }
            else {
                if (msg.size() >= 70) {
                    char buf[100];
                    for (int q = 3; q < 30; q++) {
                        sprintf(buf, "%02x ", (unsigned char)msg[q]);
                        start += buf;
                    }
                    start += "... ";
                    for (int q = -20; q < 0; q++) {
                        sprintf(buf, "%02x ", (unsigned char)msg[msg.size() - 1 + q]);
                        start += buf;
                    }
                }
                flog::info("BIN: {} bytes: {}", (int64_t)msg.size(), start);
            }
        };
        lastPing = currentTimeMillis();
        wsClient.onEveryReceive = [&]() {
            if (currentTimeMillis() - lastPing > 4000) {
                wsClient.sendString("SET keepalive");
                lastPing = currentTimeMillis();
            }
        };


    }
    void tune(double freq) {
        char buf[1024];
        sprintf(buf, "SET mod=iq low_cut=-7000 high_cut=7000 freq=%0.3f", freq / 1000.0);
        wsClient.sendString(buf);
    }

    void stop() {
        strcpy(connectionStatus, "Disconnecting..");
        wsClient.stopSocket();
    }

    void start() {
        strcpy(connectionStatus, "Connecting..");
        std::thread looper([=]() {
            flog::info("calling x.connectAndReceiveLoop..");
            try {
                std::string hostName;
                int port;
                std::size_t colonPosition = hostPort.find(":");
                if (colonPosition != std::string::npos) {
                    hostName = hostPort.substr(0, colonPosition);
                    port = std::stoi(hostPort.substr(colonPosition + 1));
                } else {
                    hostName = hostPort;
                    port = 0;
                }
                wsClient.connectAndReceiveLoop(hostName, port, "/kiwi/" + std::to_string(currentTimeMillis()) + "/SND");
                flog::info("x.connectAndReceiveLoop exited.");
                strcpy(connectionStatus, "Disconnected");
            }
            catch (const std::runtime_error& e) {
                flog::error("KiwiSDRSourceModule: Exception: {}", e.what());
                strcpy(connectionStatus, "Error: ");
                strcat(connectionStatus, e.what());
                running = false;
            }
        });
        looper.detach();
    }
};

ConfigManager config;

struct KiwiSDRSourceModule : public ModuleManager::Instance {

    std::string kiwisdrSite = "sk6ag1.ddns.net:8071";
    //    std::string kiwisdrSite = "kiwi-iva.aprs.fi";
    KiwiSDRClient kiwiSdrClient;

    KiwiSDRSourceModule(std::string name) : kiwiSdrClient() {
        this->name = name;


        kiwiSdrClient.init(kiwisdrSite, lastTuneFrequency);

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

        kiwiSdrClient.onConnected = [&]() {
            connected = true;
            tune(lastTuneFrequency, this);
        };

        kiwiSdrClient.onDisconnected = [&]() {
            connected = false;
            gui::mainWindow.setPlayState(false);
        };



        // Load config

        sigpath::sourceManager.registerSource("KiwiSDR", &handler);
    }

    ~KiwiSDRSourceModule() {
        stop(this);
        sigpath::sourceManager.unregisterSource("KiwiSDR");
    }

    void postInit() {
        geoMap.loadFrom(config, "selectmap_");
    }

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
        _this->running = true;
        _this->kiwiSdrClient.start();
        sigpath::iqFrontEnd.setCurrentStreamTime(0); // not started
        _this->nextSend = 0;
        _this->timeSet = false;
        std::thread feeder([=]() {
            double nextSend = 0;
            while (_this->running) {
                _this->kiwiSdrClient.iqDataLock.lock();
                auto bufsize = _this->kiwiSdrClient.iqData.size();
                _this->kiwiSdrClient.iqDataLock.unlock();
                double now = (double)currentTimeMillis();
                if (nextSend == 0) {
                    if (bufsize < 200) {
                        usleep(16000); // some sleep
                        continue;      // waiting for initial batch
                    }
                    nextSend = now;
                }
                else {
                    auto delay = nextSend - now;
                    double sleepTime = delay * 1000;
                    if (sleepTime > 0) {
                        usleep(sleepTime);
                    }
                }
                std::vector<std::complex<float>> toSend;
                int bufferSize = 0;
                _this->kiwiSdrClient.iqDataLock.lock();
                if (_this->kiwiSdrClient.iqData.size() >= 200) {
                    for (int i = 0; i < 200; i++) {
                        toSend.emplace_back(_this->kiwiSdrClient.iqData[i]);
                    }
                    _this->kiwiSdrClient.iqData.erase(_this->kiwiSdrClient.iqData.begin(), _this->kiwiSdrClient.iqData.begin() + 200);
                    bufferSize = _this->kiwiSdrClient.iqData.size();
                }
                _this->kiwiSdrClient.iqDataLock.unlock();
                if (bufferSize > _this->kiwiSdrClient.NETWORK_BUFFER_SIZE) {
                    nextSend += 1000.0 / 120.0;
                }
                else {
                    nextSend += 1000.0 / 60.0;
                }
                int64_t ctm = currentTimeMillis();
                if (!toSend.empty()) {
                    //                    flog::info("{} Sending samples! buf remain = {}", ctm, bufferSize);
                    memcpy(_this->stream.writeBuf, toSend.data(), toSend.size() * sizeof(dsp::complex_t));
                    _this->stream.swap((int)toSend.size());
                }
                else {
                    nextSend = 0;
                    //                    flog::info("{} Underflow of KiwiSDR iq data!", ctm);
                }
                long long int newStreamTime = currentTimeMillis() - (bufferSize / _this->kiwiSdrClient.IQDATA_FREQUENCY) - 500; // just 500.
                if (!_this->timeSet) {
                    sigpath::iqFrontEnd.setCurrentStreamTime(newStreamTime);
                    _this->timeSet = true;
                }
                else {
                    if (sigpath::iqFrontEnd.getCurrentStreamTime() < newStreamTime) {
                        sigpath::iqFrontEnd.setCurrentStreamTime(newStreamTime);
                    }
                }
            }
        });
        feeder.detach();

        _this->running = true;
        flog::info("KiwiSDRSourceModule '{0}': Start!", _this->name);
    }

    static void stop(void* ctx) {
        KiwiSDRSourceModule* _this = (KiwiSDRSourceModule*)ctx;
        if (!_this->running) { return; }
        _this->kiwiSdrClient.stop();

        _this->running = false;
        flog::info("KiwiSDRSourceModule '{0}': Stop!", _this->name);
    }

    std::vector<dsp::complex_t> incomingBuffer;

    double nextSend = 0;

    void incomingSample(double i, double q) {
        incomingBuffer.emplace_back(dsp::complex_t{ (float)q, (float)i });
        if (incomingBuffer.size() >= 200) { // 60 times per second
            double now = (double)currentTimeMillis();
            if (nextSend == 0) {
                nextSend = now;
            }
            else {
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
    std::shared_ptr<json> serversList;
    std::string serverListError;
    std::string serverTestStatus;
    std::string serverTestError;
    bool loadingList = false;

    static void tune(double freq, void* ctx) {
        KiwiSDRSourceModule* _this = (KiwiSDRSourceModule*)ctx;
        _this->lastTuneFrequency = freq;
        if (_this->running && _this->connected) {
            _this->kiwiSdrClient.tune(freq);
        }
        flog::info("KiwiSDRSourceModule '{0}': Tune: {1}!", _this->name, freq);
    }

    geomap::GeoMap geoMap;

    std::shared_ptr<json> loadServersList() {
        // http://rx.linkfanel.net/kiwisdr_com.js
        try {

            std::string jsoncache = core::args["root"].s() + "/kiwisdr_source.receiverlist.json";

            auto status = std::filesystem::status(jsoncache);
            if (exists(status)) {
                const std::filesystem::file_time_type last_write_time = std::filesystem::last_write_time(jsoncache);
                auto last_write_time_sys_clock = std::chrono::time_point_cast<std::chrono::system_clock::duration>(last_write_time - std::filesystem::file_time_type::clock::now() + std::chrono::system_clock::now());

                if (std::chrono::system_clock::now() - last_write_time_sys_clock < std::chrono::hours(1)) {
                    std::ifstream ifs(jsoncache);
                    std::string content((std::istreambuf_iterator<char>(ifs)),
                                        (std::istreambuf_iterator<char>()));
                    return std::make_shared<json>(json::parse(content));
                }
            }

            std::string host = "rx.linkfanel.net";
            auto controlSock = net::connect(host, 80);
            auto controlHttp = net::http::Client(controlSock);

            // Make request
            net::http::RequestHeader rqhdr(net::http::METHOD_GET, "/kiwisdr_com.js", host);
            controlHttp.sendRequestHeader(rqhdr);
            net::http::ResponseHeader rshdr;
            controlHttp.recvResponseHeader(rshdr, 5000);

            flog::debug("Response from {}: {}", host, rshdr.getStatusString());
            uint8_t data[2000000];
            std::string response;
            while (true) {
                auto len = controlSock->recv(data, sizeof(data));
                if (len < 1) {
                    break;
                }
                response += std::string((char*)data, len);
                usleep(100);
            }
            controlSock->close();
            auto BEGIN = "var kiwisdr_com =";
            auto END = "},\n]\n;";
            auto beginIx = response.find(BEGIN);
            if (beginIx == std::string::npos) {
                throw std::runtime_error("Invalid response from server");
            }
            auto endIx = response.find_last_of(END);
            if (endIx == std::string::npos) {
                throw std::runtime_error("Invalid response from server");
            }
            response = response.substr(beginIx + strlen(BEGIN), endIx - strlen(END) - (beginIx + strlen(BEGIN)));
            response += "}]"; // fix trailing comma unsupported by parser

            FILE* toSave = fopen(jsoncache.c_str(), "wt");
            if (toSave) {
                fwrite(response.c_str(), 1, response.size(), toSave);
                fclose(toSave);
            }

            return std::make_shared<json>(json::parse(response));
        }
        catch (std::exception& e) {
            serverListError = e.what();
            return std::shared_ptr<json>();
        }
    }

    struct ServerEntry {
        ImVec2 gps; // -1 .. 1 etc
        std::string name;
        std::string url;
        std::string antenna;
        float maxSnr;
        float secondSnr;
        int users, usersmax;
        bool selected = false;
    };

    std::vector<ServerEntry> servers;

    static void menuHandler(void* ctx) {
        KiwiSDRSourceModule* _this = (KiwiSDRSourceModule*)ctx;

        if (doFingerButton("Choose on map")) {
            ImGui::OpenPopup("My KiwiSDR Map");
        }


        ImGui::SetNextWindowPos(ImGui::GetIO().DisplaySize * 0.125f);
        if (ImGui::BeginPopupModal("My KiwiSDR Map", nullptr, ImGuiWindowFlags_AlwaysAutoResize)) {
            // Set the modal dialog's width and height
            const ImVec2 ws = ImGui::GetIO().DisplaySize * 0.75f;
            ImGui::SetWindowSize(ws);

            ImGui::BeginChild("##geomap-kiwisdr", ws - ImVec2(0, 50), true, 0);


            _this->geoMap.draw();
            if (_this->geoMap.scaleTranslateDirty) {
                _this->geoMap.saveTo(config, "selectmap_");
            }
            if (!_this->serversList) {
                if (!_this->serverListError.empty()) {
                    ImGui::Text("%s", _this->serverListError.c_str());
                }
                else {
                    ImGui::Text("Loading KiwiSDR servers list..");
                    if (!_this->loadingList) {
                        _this->loadingList = true;
                        std::thread t([=]() {
                            _this->serversList = _this->loadServersList();

                            for (const auto& entry : *_this->serversList) {
                                ServerEntry serverEntry;

                                // Check if all required fields are present
                                if (entry.contains("gps") && entry.contains("name") && entry.contains("url") &&
                                    entry.contains("snr") && entry.contains("users") && entry.contains("users_max") && entry.contains("offline")) {

                                    if (entry["offline"].get<std::string>() == "no") {
                                        std::string gps_str = entry["gps"].get<std::string>();
                                        geomap::GeoCoordinates geo;
                                        sscanf(gps_str.c_str(), "(%lf, %lf)", &geo.latitude, &geo.longitude);
                                        serverEntry.gps = geomap::geoToCartesian(geo).toImVec2();
                                        serverEntry.name = entry["name"].get<std::string>();
                                        serverEntry.url = entry["url"].get<std::string>();
                                        if (entry.contains("antenna")) {
                                            serverEntry.antenna = entry["antenna"].get<std::string>();
                                        }
                                        sscanf(entry["snr"].get<std::string>().c_str(), "%f,%f", &serverEntry.maxSnr, &serverEntry.secondSnr);
                                        serverEntry.users = atoi(entry["users"].get<std::string>().c_str());
                                        serverEntry.usersmax = atoi(entry["users_max"].get<std::string>().c_str());
                                        _this->servers.push_back(serverEntry);
                                    }
                                }
                            }

                            std::sort(_this->servers.begin(), _this->servers.end(), [](const ServerEntry& a, const ServerEntry& b) {
                                return a.maxSnr < b.maxSnr;
                            });


                            _this->loadingList = false;
                        });
                        t.detach();
                    }
                }
            }
            else {
                ImGui::Text("Loaded servers list");
                auto sz = style::baseFont->FontSize;
                ImDrawList* drawList = ImGui::GetWindowDrawList();
                for (auto& s : _this->servers) {
                    if (s.users < s.usersmax) {
                        auto dest = _this->geoMap.recentMapToScreen(s.gps);
                        auto color = ImColor(0.3f, 0.3f, 0.3f);
                        if (s.maxSnr > 22) {
                            color = ImColor(0.0f, 1.0f, 0.0f);
                        }
                        else if (s.maxSnr > 12) {
                            color = ImColor(0.6f, 0.6f, 0.6f);
                        }
                        drawList->AddRectFilled(_this->geoMap.recentCanvasPos + dest - ImVec2(sz / 2, sz / 2), _this->geoMap.recentCanvasPos + dest + ImVec2(sz / 2, sz / 2), color, sz / 4.0f);
                        if (s.selected) {
                            drawList->AddRect(_this->geoMap.recentCanvasPos + dest - ImVec2(sz / 2, sz / 2), _this->geoMap.recentCanvasPos + dest + ImVec2(sz / 2, sz / 2), ImColor(1.0f, 1.0f, 0.0f), sz / 4.0f);
                            drawList->AddRect(_this->geoMap.recentCanvasPos + dest - ImVec2(sz / 2 - 1, sz / 2 - 1), _this->geoMap.recentCanvasPos + dest + ImVec2(sz / 2 - 1, sz / 2 - 1), ImColor(1.0f, 1.0f, 0.0f), sz / 4.0f);
                        } else {
                            drawList->AddRect(_this->geoMap.recentCanvasPos + dest - ImVec2(sz / 2, sz / 2), _this->geoMap.recentCanvasPos + dest + ImVec2(sz / 2, sz / 2), ImColor(0.0f, 0.0f, 0.0f), sz / 4.0f);
                        }
                    }
                }
                if (ImGui::IsMouseClicked(0) && ImGui::GetMouseClickedCount(0) == 1) {
                    auto clickPos = ImGui::GetMousePos() - _this->geoMap.recentCanvasPos;
                    for (int i=_this->servers.size()-1; i>=0; i--) {
                        auto &it = _this->servers[i];
                        auto dest = _this->geoMap.recentMapToScreen(it.gps);
                        auto dist = sqrt(pow(dest.x - clickPos.x, 2) + pow(dest.y - clickPos.y, 2));
                        if (dist < sz / 2) {
                            for (auto& s : _this->servers) {
                                s.selected = false;
                            }
                            auto it0 = it;
                            it0.selected = true;
                            _this->servers.emplace_back(it0);
                            _this->servers.erase(_this->servers.begin() + i);
                            break;
                        }
                    }
                }
                for (auto& s : _this->servers) {
                    if (s.selected) {
                        ImGui::Text("%s", s.name.c_str());
                        if (!s.antenna.empty()) {
                            ImGui::Text("ANT: %s", s.antenna.c_str());
                        }
                        if (!s.antenna.empty()) {
                            ImGui::Text("SNR: %d", (int)s.maxSnr);
                        }
                        if (!s.antenna.empty()) {
                            ImGui::Text("USR: %d/%d", s.users, s.usersmax);
                        }
                        if (doFingerButton("Test server")) {
                            if (_this->serverTestStatus.empty()) {
                                _this->serverTestStatus = "Testing server " + s.url+" ...";
                                std::thread tester([=]() {
                                    KiwiSDRClient testClient;
                                    bool plannedDisconnect = false;
                                    if (s.url.find("http://") == 0) {
                                        auto hostPort = s.url.substr(7);
                                        auto lastSlash = hostPort.find("/");
                                        if (lastSlash != std::string::npos) {
                                            hostPort = hostPort.substr(0, lastSlash);
                                        }
                                        _this->serverTestStatus = "Testing server " + hostPort+"...";
                                        testClient.init(hostPort, 14074000);
                                        testClient.start();
                                        testClient.onConnected = [&]() {
                                            _this->serverTestStatus = "Connected to server " + hostPort;
                                            testClient.tune(14074000);
                                        };
                                        testClient.onDisconnected = [&]() {
                                            if (plannedDisconnect) {
                                                _this->serverTestStatus = "Got some data. Server OK: "+s.url;
                                            } else {
                                                _this->serverTestStatus = "Preliminary disconnect. Server NOT OK: "+s.url;
                                            }
                                        };
                                        while(true) {
                                            testClient.iqDataLock.lock();
                                            auto bufsize = testClient.iqData.size();
//                                            flog::info("checked bufsize: {}", bufsize);
                                            testClient.iqDataLock.unlock();
                                            usleep(100000);
                                            if (bufsize > 0) {
                                                plannedDisconnect = true;
                                                break;
                                            }
                                        }
                                        testClient.stop();
                                    } else {
                                        _this->serverTestStatus = "Non-http url " + s.url;
                                    }

                                });
                                tester.detach();
                            }
                        }
                    }
                }
                if (!_this->serverTestStatus.empty()) {
                    ImGui::Text("%s", _this->serverTestStatus.c_str());
                }
                if (!_this->serverTestError.empty()) {
                    ImGui::Text("Server test error: %s", _this->serverTestError.c_str());
                }
            }

            ImGui::EndChild();
            // Display some text in the modal dialog
            ImGui::Text("This is a modal dialog box with specified width and height.");

            // Close button
            if (ImGui::Button("Close")) {
                ImGui::CloseCurrentPopup();
            }

            ImGui::EndPopup();
        }

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
        ImGui::Text("Status: %s", _this->kiwiSdrClient.connectionStatus);

        long long int cst = sigpath::iqFrontEnd.getCurrentStreamTime();
        std::time_t t = cst / 1000;
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