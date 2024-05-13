#pragma once

#include <imgui.h>
#include "demod.h"

enum DemodID {
    RADIO_DEMOD_NFM,
    RADIO_DEMOD_WFM,
    RADIO_DEMOD_AM,
    RADIO_DEMOD_DSB,
    RADIO_DEMOD_USB,
    RADIO_DEMOD_CW,
    RADIO_DEMOD_LSB,
    RADIO_DEMOD_RAW,
    _RADIO_DEMOD_COUNT,
};

struct RadioModuleInterface {


    Event<ImGuiContext *> onDrawModeButtons;

    typedef demod::Demodulator *(*demodProviderFunction)(int);

    std::vector<demodProviderFunction> demodulatorProviders;


    virtual int getSelectedDemodId() = 0;

    virtual bool selectDemodByID(DemodID id) = 0;

};
