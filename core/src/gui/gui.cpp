#include <gui/gui.h>

namespace gui {
    MobileMainWindow *pmainWindow = nullptr;
    MobileMainWindow &mainWindow = *pmainWindow;
    ImGui::WaterFall waterfall;
    FrequencySelect freqSelect;
    ThemeManager themeManager;
    Menu menu;
};