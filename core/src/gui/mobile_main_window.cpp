#define IMGUI_DEFINE_MATH_OPERATORS
#define _USE_MATH_DEFINES
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
#include "../../misc_modules/noise_reduction_logmmse/src/arrays.h"
#include "dsp/convert/stereo_to_mono.h"
#include "dsp/channel/frequency_xlator.h"
#include <math.h>

using namespace ::dsp::arrays;


struct AudioInToTransmitter : dsp::Processor<dsp::stereo_t, dsp::complex_t> {

    std::string submode;
    dsp::convert::StereoToMono s2m;
    dsp::convert::RealToComplex r2c;
    dsp::channel::FrequencyXlator xlator1;
    dsp::channel::FrequencyXlator xlator2;
    dsp::tap<dsp::complex_t> ftaps;
    dsp::filter::FIR<dsp::complex_t, dsp::complex_t> filter;
    dsp::loop::AGC<float> audioAgc;

    AudioInToTransmitter(dsp::stream<dsp::stereo_t>* in) {
        init(in);
        totalRead = 0;
    }

    int totalRead;

    void setSubmode(const std::string &submode) {
        this->submode = submode;
    }

    void start() override {
        dsp::Processor<dsp::stereo_t, dsp::complex_t>::start();

        auto bandwidth = 2500.0;
        if (this->submode == "LSB") {
            xlator1.init(NULL, -bandwidth / 2, 48000);
            xlator2.init(NULL, bandwidth / 2, 48000);
        } else {
            xlator1.init(NULL, bandwidth / 2, 48000);
            xlator2.init(NULL, -bandwidth / 2, 48000);
        }
        ftaps = dsp::taps::lowPass0<dsp::complex_t>(bandwidth/2, bandwidth/2 * 0.1, 48000);
        filter.init(NULL, ftaps);
        float agcAttack = 50.0f;
        float agcDecay = 5.0f;
        audioAgc.init(NULL, 1.0, agcAttack / bandwidth, agcDecay / bandwidth, 10e6, 2.0, 1.0);
    }

    void stop() override {
        dsp::Processor<dsp::stereo_t, dsp::complex_t>::stop();
        dsp::taps::free(ftaps);
    }

    int run() override {
        int rd = this->_in->read();
        if (rd < 0) {
            return rd;
        }
        s2m.process(rd, this->_in->readBuf, s2m.out.writeBuf);
        r2c.process(rd, s2m.out.writeBuf, r2c.out.writeBuf);
        xlator1.process(rd, r2c.out.writeBuf, xlator1.out.writeBuf);
        filter.process(rd, xlator1.out.writeBuf, filter.out.writeBuf);
//        filter.process(rd, filter.out.writeBuf, filter.out.writeBuf);
        xlator2.process(rd, filter.out.writeBuf, out.writeBuf);
//
//        audioAgc.process(rd, s2m.out.writeBuf, audioAgc.out.writeBuf);

        this->_in->flush();
        if (!out.swap(rd)) {
            spdlog::info("Does not write to output stream", totalRead);
            return -1;
        }
        return rd;
    };

};

struct AudioInToFFT : dsp::Processor<dsp::stereo_t, dsp::complex_t> {

    template <class X>
    using Arg = std::shared_ptr<X>;


    std::vector<dsp::stereo_t> inputData;
    const int FREQUENCY = 48000;
    const int FRAMERATE = 60;
    Arg<fftwPlan> plan;
    const int windowSize = FREQUENCY/FRAMERATE;
    ComplexArray inputArray;
    FloatArray hanningWin;

    AudioInToFFT(dsp::stream<dsp::stereo_t>* in) {
        init(in);
        inputData.reserve(4096);
        plan = dsp::arrays::allocateFFTWPlan(false,windowSize);
        inputArray = npzeros_c(windowSize);
        hanningWin = nphanning(windowSize);
        hanningWin = div(mul(hanningWin, windowSize), npsum(hanningWin));
    }

    ~AudioInToFFT() {
    }

    int run() override{
        int rd = this->_in->read();
        if (rd < 1) {
            return rd;
        }
        int offset = inputData.size();
        inputData.resize(inputData.size() + rd);
        std::copy(_in->readBuf + 0, _in->readBuf + rd, inputData.begin()+offset);
        if (inputData.size() >= windowSize) {
            for(int q=0; q<windowSize; q++) {
                (*inputArray)[q] = dsp::complex_t{ inputData[q].l, 0 };
            }
            static int counter = 0;
            if (counter++ % 30 == 0) {
                auto ib = (float*)(*inputArray).data();
                char buf[1000];
                sprintf(buf, "input: %f %f %f %f %f %f %f %f", ib[0], ib[1], ib[2], ib[3], ib[4], ib[5], ib[6], ib[7]);
                std::string sbuf = buf;
                spdlog::info("in qsopanel: {}", sbuf);
            }
            auto inputMul = muleach(hanningWin, inputArray);
            auto result = dsp::arrays::npfftfft(inputMul, plan);
            std::copy(result->begin(), result->end(), this->out.writeBuf);
            this->out.swap(result->size());
            inputData.erase(inputData.begin(), inputData.begin() + windowSize);
        }
        this->_in->flush();
        return rd;
    };
};

struct QSOPanel {
    void start();
    void stop();
    void draw(float currentFreq, ImGui::WaterfallVFO* pVfo);
    dsp::stream<dsp::stereo_t> audioIn;
    dsp::stream<dsp::stereo_t> audioTowardsTransmitter;
    int readSamples;
    const int fftFPS = 60;
    std::shared_ptr<AudioInToFFT> audioInToFFT;
    std::shared_ptr<AudioInToTransmitter> audioInToTransmitter;
    std::shared_ptr<std::thread> receiveBuffers;
    dsp::arrays::FloatArray currentFFTBuffer;
    std::mutex currentFFTBufferMutex;
    void handleTxButton(bool tx);
    float currentFreq;
    int txGain;
    bool enablePA;
    bool transmitting;
    void setSubmode(const std::string& submode);
    std::string submode;
};



static bool withinRadius(ImVec2 center, float radius, ImVec2 point) {
    if (isnan(point.x)) {
        return false;
    }
    float dx = point.x - center.x;
    float dy = point.y - center.y;
    return dx*dx+dy*dy < radius*radius;
}

static bool withinRect(ImVec2 size, ImVec2 point) {
    if (isnan(point.x)) {
        return false;
    }
    if (point.x < 0 || point.y < 0) return false;
    if (point.x > size.x || point.y > size.y) return false;
    return true;
}

bool MobileButton::draw() {

    bool retval = false;
    ImGui::PushFont(style::mediumFont);
    auto windowPos = ImGui::GetWindowPos();
    auto widgetPos = windowPos + ImGui::GetCursorPos();
    auto avail =  ImGui::GetContentRegionAvail();
    auto window = ImGui::GetCurrentWindow();

    auto mouseCoord = ImVec2(ImGui::GetMousePos().x - widgetPos.x, ImGui::GetMousePos().y - widgetPos.y);

//    auto diameter = std::min<float>(avail.x, avail.y);
//    float radius = diameter / 2 * this->sizeFactor;


    ImU32 bg0 = ImGui::ColorConvertFloat4ToU32(ImVec4(0.5f, 0.5f, 0.5f, 1.0f));
//    const ImVec2& buttonCenter = avail / 2;

    bool pressed = ImGui::IsAnyMouseDown();
    bool startedInside = withinRect(avail, this->pressPoint);
    if (pressed) {
        if (isnan(this->pressPoint.x)) {
            this->pressPoint = mouseCoord;
            if (withinRect(avail, this->pressPoint)) {
                this->currentlyPressed = true;      // not cleared when finger away of button
            }
        }
    } else {
        this->currentlyPressed = false;
        if (startedInside && withinRect(avail, mouseCoord)) {
            // released inside
            retval = true;
        }
        this->pressPoint = ImVec2(nan(""), nan(""));
    }

    if (pressed && startedInside && withinRect(avail, mouseCoord)) {
        bg0 = ImGui::ColorConvertFloat4ToU32(ImVec4(1.0f, 1.0f, 0.5f, 1.0f));
    }

    window->DrawList->AddRectFilled(windowPos, windowPos + avail, bg0, avail.y / 10.0);


    
//    auto ts = ImGui::CalcTextSize(this->upperText.c_str());
//    ImGui::SetCursorPos((ImVec2{avail.x, 0} - ImVec2{ts.x, 0}) / 2);
//    ImGui::Text("%s", this->upperText.c_str());

    const char* buttonTextStr = this->buttonText.c_str();
    if (this->buttonText.size() == 1) {
        ImGui::PushFont(style::bigFont);
    }
    auto ts = ImGui::CalcTextSize(buttonTextStr);
    ImGui::SetCursorPos((avail - ImVec2{ts.x, ts.y}) / 2);
    ImGui::Text("%s", buttonTextStr);
    if (this->buttonText.size() == 1) {
        ImGui::PopFont();
    }
    ImGui::PopFont();

    return retval;
}

float TheEncoder::draw(ImGui::WaterfallVFO* vfo) {
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
            } else {
                // first click!
                auto currentFreq = (vfo != NULL) ? (vfo->generalOffset + gui::waterfall.getCenterFrequency()) : gui::waterfall.getCenterFrequency();
                this->currentFrequency = (float)currentFreq;
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
        if (speed != 0) {
            speed = 0;
        }
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

void MobileMainWindow::autoDetectBand(int frequency) {
    static int lastFrequency = 0;
    static const std::string *lastFreqBand = nullptr;
    const std::string *newBand;

    if (frequency == lastFrequency) {
        newBand = lastFreqBand;
    } else {
        newBand = &this->getBand(frequency);
        lastFrequency = frequency;
        lastFreqBand = newBand;
    }
    if (*newBand != this->bandUp.upperText) {
        this->bandUp.upperText = *newBand;
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
            this->selectCurrentBand("20M", -1);
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

    float encoderWidth = 150;
    float buttonsWidth = 300;
    float statusHeight = 100;

    if (this->qsoMode) {
        buttonsWidth = 600;
    }

    const ImVec2 waterfallRegion = ImVec2(ImGui::GetContentRegionAvail().x - encoderWidth - buttonsWidth, ImGui::GetContentRegionAvail().y - statusHeight);

    lockWaterfallControls = showMenu;
    ImGui::BeginChildEx("Waterfall", ImGui::GetID("sdrpp_waterfall"), waterfallRegion, false, 0);
    auto waterfallStart = ImGui::GetCursorPos();
    gui::waterfall.draw();
    ImGui::SetCursorPos(waterfallStart + ImVec2(gui::waterfall.fftAreaMin.x+ 5 * style::uiScale, 0));
    ImGui::PushFont(style::mediumFont);
    ImGui::Text("BAND: %s", this->bandUp.upperText.c_str());
    ImGui::PopFont();

    onWaterfallDrawn.emit(GImGui);
    ImGui::EndChild();

    ImGui::SetCursorPos(cornerPos + ImVec2(gui::waterfall.fftAreaMin.x+ 5 * style::uiScale, waterfallRegion.y));
    ImGui::PushFont(style::bigFont);
    ImGui::Text("%s -> %s    zoom: %s",
                this->modeToggle.upperText.c_str(),
                this->submodeToggle.upperText.c_str(),
                this->zoomToggle.upperText.c_str());
    ImGui::PopFont();

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
    float offsetDelta = encoder.draw(vfo);
    if (offsetDelta != 0) {
        auto externalFreq = (vfo != NULL) ? (vfo->generalOffset + gui::waterfall.getCenterFrequency()) : gui::waterfall.getCenterFrequency();
        if (fabs(externalFreq - encoder.currentFrequency) > 10000) {
            // something changed! e.g. changed band when encoder was rotating
            encoder.speed = 0;
        } else {
            encoder.currentFrequency -= offsetDelta * 1000;
            auto cf = ((int)(encoder.currentFrequency / 10)) * 10.0;
            tuner::tune(tuningMode, gui::waterfall.selectedVFO, cf);
        }
    }
    auto currentFreq = (vfo != NULL) ? (vfo->generalOffset + gui::waterfall.getCenterFrequency()) : gui::waterfall.getCenterFrequency();
    this->autoDetectBand((int)currentFreq);
    ImGui::EndChild(); // encoder

    ImGui::SetCursorPos(cornerPos + ImVec2{waterfallRegion.x, 0});
    auto vertPadding = ImGui::GetStyle().WindowPadding.y;


    MobileButton *buttonsNonQso[6] = {&this->modeToggle, &this->bandUp, &this->bandDown, &this->submodeToggle, &this->zoomToggle, &this->qsoButton };
    MobileButton *buttonsQso[2] = {&this->qsoButton, &this->txButton };
    auto nButtonsQso = (int)((sizeof(buttonsQso) / sizeof(buttonsQso[0])));
    auto buttonsSpaceY = ImGui::GetContentRegionAvail().y;
    if (this->qsoMode) {
        buttonsSpaceY = ImGui::GetContentRegionAvail().y / nButtonsQso;
    }
    const ImVec2 buttonsRegion = ImVec2(buttonsWidth, buttonsSpaceY);
    ImGui::BeginChildEx("Buttons", ImGui::GetID("sdrpp_mobile_buttons"), buttonsRegion, false, ImGuiWindowFlags_NoScrollbar);
    MobileButton **buttons;
    int NBUTTONS;
    if (this->qsoMode) {
        buttons = buttonsQso;
        NBUTTONS = nButtonsQso;
    } else {
        NBUTTONS = 6;
        buttons = buttonsNonQso;
    }
    MobileButton *pressedButton = nullptr;
    for (auto b=0; b<NBUTTONS; b++) {
        char chi[100];
        sprintf(chi, "mob_button_%d", b);
        const ImVec2 childRegion = ImVec2(buttonsWidth, buttonsSpaceY/NBUTTONS-vertPadding);
        ImGui::BeginChildEx(chi, ImGui::GetID(chi), childRegion, false, 0);
        auto pressed = buttons[b]->draw();
        ImGui::EndChild();
        if (pressed) {
            pressedButton = buttons[b];
        }
    }
    ImGui::EndChild(); // buttons

    if (this->qsoMode) {
        auto cp = ImGui::GetCursorPos();
        ImGui::SetCursorPos(cp + ImVec2{waterfallRegion.x, 0});
        ImGui::BeginChildEx("QSO", ImGui::GetID("sdrpp_qso"), ImVec2(buttonsWidth, ImGui::GetContentRegionAvail().y), false, 0);
        qsoPanel->draw(currentFreq, vfo);
        ImGui::EndChild(); // buttons
    }

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
            selectCurrentBand(newBand, currentFreq);
            if (getCurrentMode() == "SSB") {
                this->selectSSBModeForBand(newBand);
            }
            updateFrequencyAfterChange();
        }
    }
    if (pressedButton == &this->modeToggle) {
        auto curr = std::find(this->modes.begin(), this->modes.end(), getCurrentMode());
        if (curr != this->modes.end()) {
            auto currIndex = curr - this->modes.begin();
            currIndex = (currIndex + 1) % this->modes.size();
            auto newMode = this->modes[currIndex];
            this->leaveBandOrMode(currentFreq);
            setCurrentMode(newMode);
            pressedButton->upperText = newMode;
            updateSubmodeAfterChange();
        }
    }
    if (pressedButton == &this->qsoButton) {
        this->qsoMode = !this->qsoMode;
        if (this->qsoMode) {
            qsoPanel->start();
        } else {
            qsoPanel->stop();
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
    qsoPanel->setSubmode(submodeToggle.upperText);
    qsoPanel->handleTxButton(this->txButton.currentlyPressed);

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

void MobileMainWindow::leaveBandOrMode(int leavingFrequency) {
    auto submode = getCurrentModeAttr("submode");
    auto leavingBand = getCurrentBand();
    setCurrentModeAttr("freq_"+submode+"_"+leavingBand, std::to_string(leavingFrequency));
    if (submode == "USB" || submode == "LSB") {
        setCurrentModeAttr("band_"+leavingBand+"_ssb", submode);
    }
}

void MobileMainWindow::selectSSBModeForBand(const std::string &band) {
    auto maybeSubmode = getCurrentModeAttr("band_"+band+"_ssb");
    if (maybeSubmode != "") {
        if (bandsLimits[band].first >= 10000000) {
            maybeSubmode = "USB";
        } else {
            maybeSubmode = "LSB";
        }
    }
    setCurrentModeAttr("submode", maybeSubmode);
    updateSubmodeAfterChange();
}

void MobileMainWindow::selectCurrentBand(const std::string &band, int leavingFrequency) {
    if (this->getCurrentBand() != "" && leavingFrequency > 0) {
        this->leaveBandOrMode(leavingFrequency);
    }
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

const std::string &MobileMainWindow::getBand(int frequency) {
    static std::string empty;
    for (auto &b : bandsLimits) {
        if (frequency >= b.second.first && frequency <= b.second.second) {
            return b.first;
        }
    }
    return empty;
}

MobileMainWindow::MobileMainWindow() : MainWindow(),
                         bandUp("14 Mhz", "+"),
                         bandDown("", "-"),
                         zoomToggle("custom", "Zoom"),
                         modeToggle("SSB", "Mode"),
                         submodeToggle("LSB", "Submode"),
                         qsoButton("", "QSO"),
                         txButton("", "TX")
{
    qsoPanel = std::make_shared<QSOPanel>();
}


void QSOPanel::start() {
    readSamples = 0;
    sigpath::sinkManager.defaultInputAudio.bindStream(&audioIn);
    audioInToFFT = std::make_shared<AudioInToFFT>(&audioIn);
    audioInToFFT->start();
    std::thread x([this]{
        while (true) {
            auto rd = audioInToFFT->out.read();
            if (rd < 0) {
                spdlog::info("audioInToFFT->out.read() causes loop break, rd={0}", rd);
                break;
            }
//            spdlog::info("audioInToFFT->out.read() exited, data={0} {1} {2} {3} {4} {5} {6} {7}",
//                         audioInToFFT->out.readBuf[0].re,
//                         audioInToFFT->out.readBuf[1].re,
//                         audioInToFFT->out.readBuf[2].re,
//                         audioInToFFT->out.readBuf[3].re,
//                         audioInToFFT->out.readBuf[0].im,
//                         audioInToFFT->out.readBuf[1].im,
//                         audioInToFFT->out.readBuf[2].im,
//                         audioInToFFT->out.readBuf[3].im
//                         );
            auto frame = std::make_shared<std::vector<dsp::complex_t>>(audioInToFFT->out.readBuf, audioInToFFT->out.readBuf + rd);
            auto floatArray = npabsolute(frame);
            currentFFTBufferMutex.lock();
            currentFFTBuffer = floatArray;
            currentFFTBufferMutex.unlock();
            audioInToFFT->out.flush();
        }
    });
    x.detach();
}


void QSOPanel::setSubmode(const std::string &submode) {
    if (this->submode != submode) {
        this->submode = submode;
    }
}

void QSOPanel::handleTxButton(bool tx) {
    sigpath::txState.emit(tx);
    if (sigpath::transmitter) {
        if (tx) {
            if (!this->transmitting) {
                this->transmitting = true;
                sigpath::sinkManager.defaultInputAudio.bindStream(&audioTowardsTransmitter);
                audioInToTransmitter = std::make_shared<AudioInToTransmitter>(&audioTowardsTransmitter);
                audioInToTransmitter->setSubmode(submode);
                audioInToTransmitter->start();
                sigpath::transmitter->setTransmitStream(&audioInToTransmitter->out);
                sigpath::transmitter->setTransmitFrequency((int)currentFreq);
                sigpath::transmitter->setTransmitStatus(true);
            }
        } else {
            sigpath::transmitter->setTransmitStatus(false);
            if (this->transmitting) {
                sigpath::sinkManager.defaultInputAudio.unbindStream(&audioTowardsTransmitter);
                this->transmitting = false;
                audioInToTransmitter->stop();
                audioInToTransmitter->out.stopReader();
            }
        }
    }
}

void QSOPanel::stop() {
    sigpath::sinkManager.defaultInputAudio.unbindStream(&audioIn);
    spdlog::info("QSOPanel::stop. Calling audioInToFFT->stop()");
    audioInToFFT->stop();
    audioInToFFT->out.stopReader();
    spdlog::info("Calling audioInToFFT->reset()");
    audioInToFFT.reset();
    spdlog::info("Reset complete");
}

void QSOPanel::draw(float currentFreq, ImGui::WaterfallVFO* pVfo) {
    this->currentFreq = currentFreq;
    currentFFTBufferMutex.lock();
    if (currentFFTBuffer) {
        ImVec2 space = ImGui::GetContentRegionAvail();
        if (space.y > 200) {
            space.y = 200;
        }
        auto mx0 = npmax(currentFFTBuffer);
        auto data = npsqrt(currentFFTBuffer);
        auto mx = npmax(data);
        // only 1/4 of spectrum
        ImGui::PlotHistogram("##fft", data->data(), currentFFTBuffer->size()/4, 0, NULL, 0, mx, space);
        ImGui::Text("Max: %f", mx0);
        if (sigpath::transmitter) {
            ImGui::Text("TX state: %d", sigpath::transmitter->getTXStatus());
            ImGui::Text("Fill level: %f", sigpath::transmitter->getFillLevel());
            ImGui::Text("SWR: %f", sigpath::transmitter->getTransmitSWR());
            ImGui::Text("PWR: %f", sigpath::transmitter->getTransmitPower());
            if (ImGui::Checkbox("Enable PA", &this->enablePA)) {
                sigpath::transmitter->setPAEnabled(this->enablePA);
            }
            ImGui::LeftLabel("TX Gain");
            ImGui::SameLine();
            if (ImGui::SliderInt("##_radio_tx_gain_", &this->txGain, 0, 255)) {
                sigpath::transmitter->setTransmitGain(this->txGain);
            }
        } else {
            ImGui::Text("No transmitter! NO WAI!");
        }
//        double aPos = fftAreaMax.y - ((latestFFT[i - 1] - fftMin) * scaleFactor);
//        double bPos = fftAreaMax.y - ((latestFFT[i] - fftMin) * scaleFactor);
//        aPos = std::clamp<double>(aPos, fftAreaMin.y + 1, fftAreaMax.y);
//        bPos = std::clamp<double>(bPos, fftAreaMin.y + 1, fftAreaMax.y);
//        window->DrawList->AddLine(ImVec2(fftAreaMin.x + i - 1, roundf(aPos)),
//                                  ImVec2(fftAreaMin.x + i, roundf(bPos)), trace, 1.0);
//        window->DrawList->AddLine(ImVec2(fftAreaMin.x + i, roundf(bPos)),
//                                  ImVec2(fftAreaMin.x + i, fftAreaMax.y), shadow, 1.0);
    }
    currentFFTBufferMutex.unlock();
}
