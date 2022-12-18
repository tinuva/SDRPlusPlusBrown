#define IMGUI_DEFINE_MATH_OPERATORS
#include <gui/mobile_main_window.h>
#include <gui/gui.h>
#include "imgui_internal.h"
#include "imgui.h"
#include <stdio.h>
#include <thread>
#include <algorithm>
#include <gui/widgets/waterfall.h>
#include <gui/icons.h>
#include <gui/widgets/bandplan.h>
#include <gui/style.h>
#include <core.h>
#include <gui/menus/display.h>
#include <gui/menus/theme.h>
#include <filesystem>
#include <gui/tuner.h>
#include "../../decoder_modules/radio/src/radio_module.h"

//using namespace std;

static bool withinRadius(ImVec2 center, float radius, ImVec2 point) {
    if (isnan(point.x)) {
        return false;
    }
    float dx = point.x - center.x;
    float dy = point.y - center.y;
    return dx*dx+dy*dy < radius*radius;
}

bool MobileButton::draw() {

    bool retval = false;
    ImGui::PushFont(style::mediumFont);
    auto windowPos = ImGui::GetWindowPos();
    auto widgetPos = windowPos + ImGui::GetCursorPos();
    auto avail =  ImGui::GetContentRegionAvail();
    auto window = ImGui::GetCurrentWindow();

    auto mouseCoord = ImVec2(ImGui::GetMousePos().x - widgetPos.x, ImGui::GetMousePos().y - widgetPos.y);

    auto diameter = std::min<float>(avail.x, avail.y);
    float radius = diameter / 2 * this->sizeFactor;


    ImU32 bg0 = ImGui::ColorConvertFloat4ToU32(ImVec4(0.5f, 0.5f, 0.5f, 1.0f));
    const ImVec2& buttonCenter = avail / 2;

    bool pressed = ImGui::IsAnyMouseDown();
    bool startedInside = withinRadius(buttonCenter, radius, this->pressPoint);
    if (pressed) {
        if (isnan(this->pressPoint.x)) {
            this->pressPoint = mouseCoord;
        }
    } else {
        if (startedInside && withinRadius(buttonCenter, radius, mouseCoord)) {
            // released inside
            retval = true;
        }
        this->pressPoint = ImVec2(nan(""), nan(""));
    }

    if (pressed && startedInside && withinRadius(buttonCenter, radius, mouseCoord)) {
        bg0 = ImGui::ColorConvertFloat4ToU32(ImVec4(1.0f, 1.0f, 0.5f, 1.0f));
    }

    window->DrawList->AddCircleFilled(windowPos + buttonCenter, radius, bg0);


    
    auto ts = ImGui::CalcTextSize(this->upperText.c_str());
    ImGui::SetCursorPos((ImVec2{avail.x, 0} - ImVec2{ts.x, 0}) / 2);
    ImGui::Text("%s", this->upperText.c_str());

    const char* buttonTextStr = this->buttonText.c_str();
    if (this->buttonText.size() == 1) {
        ImGui::PushFont(style::bigFont);
    }
    ts = ImGui::CalcTextSize(buttonTextStr);
    ImGui::SetCursorPos((avail - ImVec2{ts.x, ts.y}) / 2);
    ImGui::Text("%s", buttonTextStr);
    if (this->buttonText.size() == 1) {
        ImGui::PopFont();
    }
    ImGui::PopFont();

    return retval;
}

float TheEncoder::draw() {
    auto avail =  ImGui::GetContentRegionAvail();
    int MARKS_PER_ENCODER = 20;
    float retval = 0;


    float R = avail.y / 2;
    auto window = ImGui::GetCurrentWindow();
    auto widgetPos = ImGui::GetWindowPos();
    auto DRAW_HEIGHT_PERCENT = 0.95f;

    ImU32 bg0 = ImGui::ColorConvertFloat4ToU32(ImVec4(0.0f, 0.0f, 0.0f, 1.0f));
    window->DrawList->AddRectFilled(widgetPos, widgetPos + avail, bg0);

    for(int q = 0; q<MARKS_PER_ENCODER; q++) {
        auto markY = (R* DRAW_HEIGHT_PERCENT) *(float)sin(M_PI*2 / MARKS_PER_ENCODER * q + this->somePosition);
        auto markColor = (float)cos(M_PI*2 / MARKS_PER_ENCODER * q + this->somePosition);
        if (markColor > 0) {
            ImU32 col = ImGui::ColorConvertFloat4ToU32(ImVec4(1, 1, 1, markColor));
            window->DrawList->AddRectFilled(widgetPos + ImVec2(0, R + markY - 4), widgetPos + ImVec2(avail.x, R + markY + 4), col);
        }
    }

    if (ImGui::IsAnyMouseDown()) {
        auto mouseX = ImGui::GetMousePos().x - widgetPos.x;
        bool mouseInside = mouseX >= 0 && mouseX < avail.x;
        if (!mouseInside && isnan(lastMouseAngle)) {
            // clicked outside
        } else {
            // can continue everywhere
            auto mouseAngle = (ImGui::GetMousePos().y - widgetPos.y - R) / (R * DRAW_HEIGHT_PERCENT);
            if (!isnan(lastMouseAngle)) {
                this->somePosition += mouseAngle - lastMouseAngle;
                retval = mouseAngle - lastMouseAngle;
            }
            lastMouseAngle = mouseAngle;
            this->speed = 0;
            fingerMovement.emplace_back(lastMouseAngle);
        }
    } else {
        auto sz = this->fingerMovement.size();
        if (sz >= 2) {
            this->speed = (this->fingerMovement[sz-1] - this->fingerMovement[sz-2])/2;
        }
        this->fingerMovement.clear();
        lastMouseAngle = nan("");
    }
    if (fabs(speed) < 0.001) {
        speed = 0;
    }
    this->somePosition += speed;
    if (speed != 0) {
        retval = speed;
    }
    this->speed *= this->delayFactor;
    return retval;

}

void MobileMainWindow::updateSubmodeAfterChange() {
    auto mode = getCurrentMode();
    auto submode = getCurrentModeAttr("submode");
    if (submode.empty()) {
        submode = subModes[mode].front();
        setCurrentModeAttr("submode", submode);
    }
    this->submodeToggle.upperText = submode;
    ImGui::WaterfallVFO*& pVfo = gui::waterfall.vfos[gui::waterfall.selectedVFO];
    if (pVfo) {
        auto selectedDemod = RadioModule::RADIO_DEMOD_USB;
        if (submode == "LSB") selectedDemod = RadioModule::RADIO_DEMOD_LSB;
        if (submode == "CWU") selectedDemod = RadioModule::RADIO_DEMOD_CW;
        if (submode == "CWL") selectedDemod = RadioModule::RADIO_DEMOD_CW;
        if (submode == "NFM") selectedDemod = RadioModule::RADIO_DEMOD_NFM;
        if (submode == "WFM") selectedDemod = RadioModule::RADIO_DEMOD_WFM;
        if (submode == "AM") selectedDemod = RadioModule::RADIO_DEMOD_AM;
        pVfo->onUserChangedDemodulator.emit((int)selectedDemod);
    }
    updateFrequencyAfterChange();
}

void MobileMainWindow::updateFrequencyAfterChange() {
    auto mode = getCurrentMode();
    auto submode = getCurrentModeAttr("submode");
    auto band = getCurrentBand();
    auto maybeFreq = getCurrentModeAttr("freq_"+submode+"_"+band);
    this->bandUp.upperText = band;
    if (maybeFreq != "") {
        double currentFreq = atof(maybeFreq.c_str());
        tuner::tune(tuner::TUNER_MODE_CENTER, gui::waterfall.selectedVFO, currentFreq);
        return;
    }
    auto nfreq = this->frequencyDefaults[band+"_"+submode];
    if (nfreq == 0)
        nfreq = this->frequencyDefaults[band+"_"+mode];
    if (nfreq == 0)
        nfreq = this->frequencyDefaults[band];
    if (nfreq != 0) {
        tuner::tune(tuner::TUNER_MODE_CENTER, gui::waterfall.selectedVFO, nfreq*1000.0);
    }
}

void MobileMainWindow::draw() {
    gui::waterfall.alwaysDrawLine = false;
    if (!displaymenu::smallScreen) {
        this->shouldInitialize = true;
        MainWindow::draw();
        return;
    }
    if (this->shouldInitialize) {
        this->shouldInitialize = false;
        if (this->getCurrentBand() == "") {
            this->setCurrentBand("20M");
        }
        this->bandUp.upperText = this->getCurrentBand();
        auto currentMode = this->getCurrentMode();
        this->modeToggle.upperText = currentMode;
        updateSubmodeAfterChange();
        zoomToggle.upperText = "custom";
    }
    gui::waterfall.alwaysDrawLine = true;
    ImGui::WaterfallVFO* vfo;
    ImVec2 winSize = ImGui::GetWindowSize();
    this->preDraw(vfo);

    ImGui::Begin("Main", NULL, WINDOW_FLAGS);

    ImVec4 textCol = ImGui::GetStyleColorVec4(ImGuiCol_Text);
    ImVec2 btnSize(30 * style::uiScale, 30 * style::uiScale);
    ImGui::PushID(ImGui::GetID("sdrpp_menu_btn"));
    if (ImGui::ImageButton(icons::MENU, btnSize, ImVec2(0, 0), ImVec2(1, 1), 5, ImVec4(0, 0, 0, 0), textCol) || ImGui::IsKeyPressed(ImGuiKey_Menu, false)) {
        showMenu = !showMenu;
        core::configManager.acquire();
        core::configManager.conf["showMenu"] = showMenu;
        core::configManager.release(true);
    }
    ImGui::PopID();

    ImGui::SameLine();

    this->drawUpperLine(vfo);

    auto cornerPos = ImGui::GetCursorPos();

    float encoderWidth = 300;
    float buttonsWidth = 300;

    const ImVec2 waterfallRegion = ImVec2(ImGui::GetContentRegionAvail().x - encoderWidth - buttonsWidth, ImGui::GetContentRegionAvail().y);

    lockWaterfallControls = showMenu;
    ImGui::BeginChildEx("Waterfall", ImGui::GetID("sdrpp_waterfall"), waterfallRegion, false, 0);
    gui::waterfall.draw();
    onWaterfallDrawn.emit(GImGui);
    ImGui::EndChild();

    ImGui::SetCursorPos(cornerPos);

    if (showMenu) {
        menuWidth = core::configManager.conf["menuWidth"];
        const ImVec2 menuRegion = ImVec2(menuWidth, ImGui::GetContentRegionAvail().y);
        ImGui::BeginChildEx("Menu", ImGui::GetID("sdrpp_menu"), menuRegion, false, 0);
        ImU32 bg = ImGui::ColorConvertFloat4ToU32(gui::themeManager.waterfallBg);
        auto window = ImGui::GetCurrentWindow();
        auto widgetPos = ImGui::GetWindowContentRegionMin();
        window->DrawList->AddRectFilled(widgetPos, widgetPos + menuRegion, bg);

        if (gui::menu.draw(firstMenuRender)) {
            core::configManager.acquire();
            json arr = json::array();
            for (int i = 0; i < gui::menu.order.size(); i++) {
                arr[i]["name"] = gui::menu.order[i].name;
                arr[i]["open"] = gui::menu.order[i].open;
            }
            core::configManager.conf["menuElements"] = arr;

            // Update enabled and disabled modules
            for (auto [_name, inst] : core::moduleManager.instances) {
                if (!core::configManager.conf["moduleInstances"].contains(_name)) { continue; }
                core::configManager.conf["moduleInstances"][_name]["enabled"] = inst.instance->isEnabled();
            }

            core::configManager.release(true);
        }
        if (startedWithMenuClosed) {
            startedWithMenuClosed = false;
        }
        else {
            firstMenuRender = false;
        }
        ImGui::EndChild();

    }

    ImGui::SetCursorPos(cornerPos + ImVec2{waterfallRegion.x + buttonsWidth, 0});
    const ImVec2 encoderRegion = ImVec2(encoderWidth, ImGui::GetContentRegionAvail().y);
    ImGui::BeginChildEx("Encoder", ImGui::GetID("sdrpp_encoder"), encoderRegion, false, 0);
    float offsetDelta = encoder.draw();
    if (offsetDelta != 0) {
        auto currentFreq = (vfo != NULL) ? (vfo->generalOffset + gui::waterfall.getCenterFrequency()) : gui::waterfall.getCenterFrequency();
        currentFreq -= offsetDelta * 1000;
        auto cf = ((int)(currentFreq / 10)) * 10.0;
        tuner::tune(tuningMode, gui::waterfall.selectedVFO, cf);

    }
    onWaterfallDrawn.emit(GImGui);
    ImGui::EndChild();

    ImGui::SetCursorPos(cornerPos + ImVec2{waterfallRegion.x, 0});
    auto buttonsSpaceY = ImGui::GetContentRegionAvail().y - 30;
    const ImVec2 buttonsRegion = ImVec2(buttonsWidth, buttonsSpaceY);
    ImGui::BeginChildEx("Buttons", ImGui::GetID("sdrpp_mobile_buttons"), buttonsRegion, false, ImGuiWindowFlags_NoScrollbar);

    const int NBUTTONS = 6;
    MobileButton *buttons[NBUTTONS] = {&this->modeToggle, &this->bandUp, &this->bandDown, &this->submodeToggle, &this->zoomToggle, &this->txButton};
    MobileButton *pressedButton = nullptr;
    for (auto b=0; b<NBUTTONS; b++) {
        char chi[100];
        sprintf(chi, "mob_button_%d", b);
        const ImVec2 childRegion = ImVec2(buttonsWidth, buttonsSpaceY/NBUTTONS);
        ImGui::BeginChildEx(chi, ImGui::GetID(chi), childRegion, false, 0);
        auto pressed = buttons[b]->draw();
        ImGui::EndChild();
        if (pressed) {
            pressedButton = buttons[b];
        }
    }


    ImGui::EndChild();

    ImGui::End();

    if (pressedButton == &this->zoomToggle) {
        int selectedIndex = -1;
        for(auto q = 0; q<zooms.size(); q++) {
            if (zooms[q].first == zoomToggle.upperText) {
                selectedIndex = q;
                break;
            }
        }
        selectedIndex = (selectedIndex + 1)%zooms.size();
        zoomToggle.upperText = zooms[selectedIndex].first;
        updateWaterfallZoomBandwidth(zooms[selectedIndex].second);
        double wfBw = gui::waterfall.getBandwidth();
        auto nbw = wfBw;
        if (zooms[selectedIndex].second != 0) {
            nbw = zooms[selectedIndex].second;
        }
        if (nbw > wfBw) {
            nbw = wfBw;
        }
        gui::waterfall.setViewBandwidth(nbw);
        if (vfo != NULL) {
            gui::waterfall.setViewOffset(vfo->centerOffset); // center vfo on screen
        }
    }

    if (pressedButton == &this->bandUp || pressedButton == &this->bandDown) {
        auto cb = getCurrentBand();
        auto cbIter = std::find(bands.begin(), bands.end(), cb);
        if (cbIter != bands.end()) {
            auto cbIndex = cbIter - bands.begin();
            if (pressedButton == &this->bandUp) {
                cbIndex = (cbIndex + 1) % bands.size();
            } else {
                cbIndex = (cbIndex + bands.size()-1) % bands.size();
            }
            auto newBand = bands[cbIndex];
            setCurrentBand(newBand);
            updateFrequencyAfterChange();
        }
    }
    if (pressedButton == &this->modeToggle) {
        auto curr = std::find(this->modes.begin(), this->modes.end(), getCurrentMode());
        if (curr != this->modes.end()) {
            auto currIndex = curr - this->modes.begin();
            currIndex = (currIndex + 1) % this->modes.size();
            auto newMode = this->modes[currIndex];
            setCurrentMode(newMode);
            pressedButton->upperText = newMode;
            updateSubmodeAfterChange();
        }
    }
    if (pressedButton == &this->submodeToggle) {
        std::vector<std::string>& submos = subModes[getCurrentMode()];
        auto submoIter = std::find(submos.begin(), submos.end(), getCurrentModeAttr("submode"));
        if (submoIter != submos.end()) {
            auto submoIndex = submoIter - submos.begin();
            submoIndex = (submoIndex + 1) % submos.size();
            auto newSubmode = submos[submoIndex];
            setCurrentModeAttr("submode", newSubmode);
            updateSubmodeAfterChange();
        }
    }


}
std::string MobileMainWindow::getCurrentBand() {
    std::string retval;
    core::configManager.acquire();
    if (core::configManager.conf.find("mobileBand") == core::configManager.conf.end()) {
        core::configManager.release(false);
        return "20M";
    }
    retval = core::configManager.conf["mobileBand"];
    core::configManager.release(false);
    return retval;
}

std::string MobileMainWindow::getCurrentMode() {
    bool changed = false;
    std::string retval;
    core::configManager.acquire();
    if (core::configManager.conf.find("mobileRadioMode") == core::configManager.conf.end()) {
        core::configManager.conf["mobileRadioMode"] = this->modes.front();
        changed = true;
    }
    retval = core::configManager.conf["mobileRadioMode"];
    core::configManager.release(changed);
    return retval;
}

void MobileMainWindow::setCurrentMode(std::string mode) {
    core::configManager.acquire();
    core::configManager.conf["mobileRadioMode"] = mode;
    core::configManager.release(true);
}

void MobileMainWindow::setCurrentBand(std::string band) {
    core::configManager.acquire();
    core::configManager.conf["mobileBand"] = band;
    core::configManager.release(true);
}

std::string MobileMainWindow::getCurrentModeAttr(std::string key) {
    std::string retval;
    auto currentMode = getCurrentMode();
    core::configManager.acquire();
    json x = core::configManager.conf["mobileRadioMode_"+currentMode];
    if (!x.empty()) {
        if (x.find(key) != x.end()) {
            retval = x[key];
        }
    }
    core::configManager.release(false);
    return retval;
}

void MobileMainWindow::setCurrentModeAttr(std::string key, std::string val) {
    auto currentMode = getCurrentMode();
    core::configManager.acquire();
    json x = core::configManager.conf["mobileRadioMode_"+currentMode];
    if (!x.empty()) {
        x[key] = val;
        core::configManager.conf["mobileRadioMode_"+currentMode] = x;
    } else {
        x = json();
        x[key] = val;
        core::configManager.conf["mobileRadioMode_"+currentMode] = x;
    }
    core::configManager.release(true);
}
