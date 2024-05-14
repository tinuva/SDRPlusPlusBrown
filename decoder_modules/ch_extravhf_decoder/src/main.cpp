
#include <itpp/itcomm.h>


#include <imgui.h>
#include <module.h>
#include <gui/gui.h>
#include <gui/style.h>
#include <signal_path/signal_path.h>
#include "../../radio/src/radio_module_interface.h"
#include <config.h>
#include <core.h>
#include <utils/optionlist.h>
#include "./demod.h"

ConfigManager config;

#define CONCAT(a, b) ((std::string(a) + b).c_str())


class VhfVoiceRadioModule : public ModuleManager::Instance {
public:

    std::string name;

    VhfVoiceRadioModule(std::string name) {

    }

    ~VhfVoiceRadioModule() {
    }


    struct Injection {
        RadioModuleInterface *radio;
        VhfVoiceRadioModule *thiz;
        EventHandler<ImGuiContext *> drawModeButtonsHandler;
        RadioModuleInterface::demodProviderFunction demodFunction;
    };

    static const int RADIO_DEMOD_DSD = 0x1301;
    static const int RADIO_DEMOD_OLDDSD = 0x1302;

    static void onDrawModeButtons(ImGuiContext *imguiContext, void *ctx) {
        auto inj = (Injection *)ctx;
        auto _this = inj->thiz;
        if (ImGui::RadioButton(CONCAT("DSD##_", _this->name), inj->radio->getSelectedDemodId() == RADIO_DEMOD_DSD) && inj->radio->getSelectedDemodId() != RADIO_DEMOD_DSD) {
            inj->radio->selectDemodByID((DemodID )RADIO_DEMOD_DSD);
        }
        ImGui::SameLine();
        if (ImGui::RadioButton(CONCAT("OLD DSD##_", _this->name), inj->radio->getSelectedDemodId() == RADIO_DEMOD_OLDDSD) && inj->radio->getSelectedDemodId() != RADIO_DEMOD_OLDDSD) {
            inj->radio->selectDemodByID((DemodID)RADIO_DEMOD_OLDDSD);
        }
    }

    bool injected = false;

    std::vector<std::shared_ptr<Injection>> injections;

    std::shared_ptr<Injection> getInjection(RadioModuleInterface *radio) {
        for(auto& x: injections) {
            if (x->radio == radio) {
                return x;
            }
        }
        auto rv =std::make_shared<Injection>();
        injections.emplace_back(rv);
        rv->radio = radio;
        rv->thiz = this;
        rv->demodFunction = [](int id) -> demod::Demodulator* {
            switch (id) {
                case RADIO_DEMOD_DSD:  return new demod::DSD(); break;
                case RADIO_DEMOD_OLDDSD:  return new demod::OldDSD(); break;
                default:                        return NULL; break;
            }
        };
        return rv;
    }

    void inject() {
        if (!injected) {
            injected = true;

            for(auto x: core::moduleManager.instances) {
                Instance *pInstance = x.second.instance;
                auto radio = (RadioModuleInterface *)pInstance->getInterface("RadioModuleInterface");
                if (radio) {
                    auto inj = getInjection(radio);
                    inj->drawModeButtonsHandler.ctx = inj.get();
                    inj->drawModeButtonsHandler.handler = onDrawModeButtons;
                    radio->onDrawModeButtons.bindHandler(&inj->drawModeButtonsHandler);
                    radio->demodulatorProviders.emplace_back(inj->demodFunction);
                }
            }
        }
    }

    void uninject() {
        if (injected) {
            injected = false;

            for (auto x: core::moduleManager.instances) {
                Instance *pInstance = x.second.instance;
                auto radio = (RadioModuleInterface *) pInstance->getInterface("RadioModuleInterface");
                if (radio) {
                    auto inj = getInjection(radio);
                    radio->onDrawModeButtons.unbindHandler(&inj->drawModeButtonsHandler);
                    auto wh = std::find(radio->demodulatorProviders.begin(), radio->demodulatorProviders.end(), inj->demodFunction);
                    if (wh != radio->demodulatorProviders.end()) {
                        radio->demodulatorProviders.erase(wh);
                    }
                }
            }
        }
    }

    void postInit() {
        inject();
    }

    void enable() {
        enabled = true;
        inject();
    }

    void disable() {
        enabled = false;
        uninject();
    }

    bool isEnabled() {
        return enabled;
    }


private:
    static void menuHandler(void* ctx) {
        ImGui::Text("See new modes in Radio.");
        ImGui::Text("These modes are provided");
        ImGui::Text("for educational purposes.");
    }

    bool enabled = true;
};


SDRPP_MOD_INFO{
    /* Name:            */ "ch_extravhf_decoder",
    /* Description:     */ "Additional modes for V/UHF voice",
    /* Author:          */ "cropinghigh",
    /* Version:         */ 0, 0, 6,
    /* Max instances    */ -1
};

MOD_EXPORT void _INIT_() {
    json def = json({});
    config.setPath(core::args["root"].s() + "/ch_extravhf_decoder_config.json");
    config.load(def);
    config.enableAutoSave();
}

MOD_EXPORT ModuleManager::Instance* _CREATE_INSTANCE_(std::string name) {
    return new VhfVoiceRadioModule(name);
}

MOD_EXPORT void _DELETE_INSTANCE_(void* instance) {
    auto mymod = (VhfVoiceRadioModule*)instance;
    mymod->uninject();
    delete mymod;
}

MOD_EXPORT void _END_() {
    config.disableAutoSave();
    config.save();
}
