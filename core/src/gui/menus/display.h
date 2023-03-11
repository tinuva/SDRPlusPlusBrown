#pragma once

namespace displaymenu {
    void init();
    void draw(void* ctx);
    extern bool smallScreen;
#ifdef __ANDROID__
    extern float displayDensity;
#endif
    extern enum TranscieverLayout {
        TRAL_NONE = 0,
        TRAL_SSB_FIRST = 1
    } transcieverLayout;

}