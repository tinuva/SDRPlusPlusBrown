
#ifdef _WIN32
#define _WINSOCKAPI_ // stops windows.h including winsock.h
#endif

#define IMGUI_DEFINE_MATH_OPERATORS
#include <imgui.h>

#include "kiwisdr_map.h"

std::shared_ptr<ConfigManager> kiwiSDRMapConfig;
std::shared_ptr<KiwiSDRMapSelector> kiwiSDRMapSelector;

