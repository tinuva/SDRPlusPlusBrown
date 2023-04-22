#pragma once

#include "utils/event.h"
#include <imgui/imgui.h>

namespace displaymenu {
    void init();
    void draw(void* ctx);
    extern bool smallScreen;
#ifdef __ANDROID__
    extern float displayDensity;
#endif
    extern Event<ImGuiContext *> onDisplayDraw;

    extern enum TranscieverLayout {
        TRAL_NONE = 0,
        TRAL_SSB_FIRST = 1
    } transcieverLayout;

}