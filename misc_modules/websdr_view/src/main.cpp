#include <utils/networking.h>
#include <imgui.h>
#include <module.h>
#include <gui/gui.h>
#include <core.h>
#include <config.h>

#define MAX_COMMAND_LENGTH 8192

SDRPP_MOD_INFO{
    /* Name:            */ "websdr_view",
    /* Description:     */ "View Multiple Websdr",
    /* Author:          */ "San",
    /* Version:         */ 0, 1, 0,
    /* Max instances    */ -1
};

ConfigManager config;

class WebsdrViewModule : public ModuleManager::Instance {

    bool intlSupport;
    bool mouseWheel;
    bool zoomSave;

public:
    WebsdrViewModule(std::string name) {
        this->name = name;

//        config.acquire();
//        if (!config.conf.contains(name)) {
//            config.conf[name]["intl_support"] = true;
//            config.conf[name]["mouse_wheel"] = true;
//            config.conf[name]["zoom_save"] = true;
//            config.conf[name]["snr_meter"] = true;
//        }
//        intlSupport = config.conf[name]["intl_support"];
//        mouseWheel = config.conf[name]["mouse_wheel"];
//        zoomSave = config.conf[name]["zoom_save"];
//
//        config.release(true);

        gui::menu.registerEntry(name, menuHandler, this, NULL);
    }

    ~WebsdrViewModule() {
        gui::menu.removeEntry(name);
    }

    void postInit() {
        // Refresh modules

        // If autostart is enabled, start the server
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

private:
    static void menuHandler(void* ctx) {
        WebsdrViewModule* _this = (WebsdrViewModule*)ctx;
    }

    std::string name;
    bool enabled = true;

};

MOD_EXPORT void _INIT_() {
    config.setPath(core::args["root"].s() + "/websdr_view.json");
    config.load(json::object());
    config.enableAutoSave();
}

MOD_EXPORT ModuleManager::Instance* _CREATE_INSTANCE_(std::string name) {
    return new WebsdrViewModule(name);
}

MOD_EXPORT void _DELETE_INSTANCE_(void* instance) {
    delete (WebsdrViewModule*)instance;
}

MOD_EXPORT void _END_() {
    config.disableAutoSave();
    config.save();
}
