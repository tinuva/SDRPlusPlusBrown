#include "sdrpp_server_client.h"
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


SDRPP_MOD_INFO{
    /* Name:            */ "kiwisdr_source",
    /* Description:     */ "KiwiSDR WebSDR source module for SDR++",
    /* Author:          */ "san",
    /* Version:         */ 0, 1, 0,
    /* Max instances    */ 1
};

struct KiwiSDRClient {

};

ConfigManager config;

class KiwiSDRSourceModule : public ModuleManager::Instance {
public:
    KiwiSDRSourceModule(std::string name) {
        this->name = name;

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

private:

    static void menuSelected(void* ctx) {
        KiwiSDRSourceModule* _this = (KiwiSDRSourceModule*)ctx;
        gui::mainWindow.playButtonLocked = true;
        flog::info("KiwiSDRSourceModule '{0}': Menu Select!", _this->name);
    }

    static void menuDeselected(void* ctx) {
        KiwiSDRSourceModule* _this = (KiwiSDRSourceModule*)ctx;
        gui::mainWindow.playButtonLocked = false;
        flog::info("KiwiSDRSourceModule '{0}': Menu Deselect!", _this->name);
    }

    static void start(void* ctx) {
        KiwiSDRSourceModule* _this = (KiwiSDRSourceModule*)ctx;
        if (_this->running) { return; }

//        // Try to connect if not already connected
//        if (!_this->client) {
//            _this->tryConnect();
//            if (!_this->client) { return; }
//        }
//
//        // Set configuration
//        _this->client->setFrequency(_this->freq);
//        _this->client->start();

        _this->running = true;
        flog::info("KiwiSDRSourceModule '{0}': Start!", _this->name);
    }

    static void stop(void* ctx) {
        KiwiSDRSourceModule* _this = (KiwiSDRSourceModule*)ctx;
        if (!_this->running) { return; }

//        if (_this->client) { _this->client->stop(); }

        _this->running = false;
        flog::info("KiwiSDRSourceModule '{0}': Stop!", _this->name);
    }

    static void tune(double freq, void* ctx) {
        KiwiSDRSourceModule* _this = (KiwiSDRSourceModule*)ctx;
//        if (_this->running && _this->client) {
//            _this->client->setFrequency(freq);
//        }
//        _this->freq = freq;
        flog::info("KiwiSDRSourceModule '{0}': Tune: {1}!", _this->name, freq);
    }

    static void menuHandler(void* ctx) {
        KiwiSDRSourceModule* _this = (KiwiSDRSourceModule*)ctx;

        if (ImGui::Button("Select server"))
//        float menuWidth = ImGui::GetContentRegionAvail().x;

////        bool connected = (_this->client && _this->client->isOpen());
////        gui::mainWindow.playButtonLocked = !connected;
//
////        ImGui::GenericDialog("##sdrpp_srv_src_err_dialog", _this->serverBusy, GENERIC_DIALOG_BUTTONS_OK, [=](){
////            ImGui::TextUnformatted("This server is already in use.");
////        });
//
//        if (connected) { style::beginDisabled(); }
//        if (ImGui::InputText(CONCAT("##sdrpp_srv_srv_host_", _this->name), _this->hostname, 1023)) {
//            config.acquire();
//            config.conf["hostname"] = _this->hostname;
//            config.release(true);
//        }
//        ImGui::SameLine();
//        ImGui::SetNextItemWidth(menuWidth - ImGui::GetCursorPosX());
//        if (ImGui::InputInt(CONCAT("##sdrpp_srv_srv_port_", _this->name), &_this->port, 0, 0)) {
//            config.acquire();
//            config.conf["port"] = _this->port;
//            config.release(true);
//        }
//        if (connected) { style::endDisabled(); }
//
//        if (_this->running) { style::beginDisabled(); }
//        if (!connected && ImGui::Button("Connect##sdrpp_srv_source", ImVec2(menuWidth, 0))) {
//            _this->tryConnect();
//        }
//        else if (connected && ImGui::Button("Disconnect##sdrpp_srv_source", ImVec2(menuWidth, 0))) {
//            _this->client->close();
//        }
//        if (_this->running) { style::endDisabled(); }
//
//
//        if (connected) {
//            ImGui::LeftLabel("Sample type");
//            ImGui::FillWidth();
//            if (ImGui::Combo("##sdrpp_srv_source_samp_type", &_this->sampleTypeId, _this->sampleTypeList.txt)) {
//                _this->client->setSampleType(_this->sampleTypeList[_this->sampleTypeId]);
//
//                // Save config
//                config.acquire();
//                config.conf["servers"][_this->devConfName]["sampleType"] = _this->sampleTypeList.key(_this->sampleTypeId);
//                config.release(true);
//            }
//
//            if (ImGui::Checkbox("Compression", &_this->compression)) {
//                _this->client->setCompression(_this->compression);
//
//                // Save config
//                config.acquire();
//                config.conf["servers"][_this->devConfName]["compression"] = _this->compression;
//                config.release(true);
//            }
//
//            bool dummy = true;
//            style::beginDisabled();
//            ImGui::Checkbox("Full IQ", &dummy);
//            style::endDisabled();
//
//            // Calculate datarate
//            _this->frametimeCounter += ImGui::GetIO().DeltaTime;
//            if (_this->frametimeCounter >= 0.2f) {
//                _this->datarate = ((float)_this->client->bytes / (_this->frametimeCounter * 1024.0f * 1024.0f)) * 8;
//                _this->frametimeCounter = 0;
//                _this->client->bytes = 0;
//            }
//
//            ImGui::TextUnformatted("Status:");
//            ImGui::SameLine();
//            ImGui::TextColored(ImVec4(0.0f, 1.0f, 0.0f, 1.0f), "Connected (%.3f Mbit/s)", _this->datarate);
//
//            ImGui::CollapsingHeader("Source [REMOTE]", ImGuiTreeNodeFlags_DefaultOpen);
//
//            _this->client->showMenu();
//        }
//        else {
//            ImGui::TextUnformatted("Status:");
//            ImGui::SameLine();
//            ImGui::TextUnformatted("Not connected (--.--- Mbit/s)");
//        }
    }

    void tryConnect() {
        try {
            if (client) { client.reset(); }
            client = server::connect(hostname, port, &stream);
            deviceInit();
        }
        catch (std::exception e) {
            flog::error("Could not connect to SDR: {0}", e.what());
            if (!strcmp(e.what(), "Server busy")) { serverBusy = true; }
        }
    }

    void deviceInit() {
        // Generate the config name
        char buf[4096];
        sprintf(buf, "%s:%05d", hostname, port);
        devConfName = buf;

        // Load settings
        sampleTypeId = sampleTypeList.valueId(dsp::compression::PCM_TYPE_I16);
        if (config.conf["servers"][devConfName].contains("sampleType")) {
            std::string key = config.conf["servers"][devConfName]["sampleType"];
            if (sampleTypeList.keyExists(key)) { sampleTypeId = sampleTypeList.keyId(key); }
        }
        if (config.conf["servers"][devConfName].contains("compression")) {
            compression = config.conf["servers"][devConfName]["compression"];
        }

        // Set settings
        client->setSampleType(sampleTypeList[sampleTypeId]);
        client->setCompression(compression);
    }

    std::string name;
    bool enabled = true;
    bool running = false;
    
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