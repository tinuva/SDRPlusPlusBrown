#pragma once

namespace displaymenu {
    void init();
    void draw(void* ctx);
    extern bool smallScreen;
#ifdef __ANDROID__
    extern float displayDensity;
#endif
}