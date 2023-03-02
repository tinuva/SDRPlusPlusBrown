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
#include "ctm.h"
#include <math.h>

using namespace ::dsp::arrays;

static bool withinRect(ImVec2 size, ImVec2 point) {
    if (isnan(point.x)) {
        return false;
    }
    if (point.x < 0 || point.y < 0) return false;
    if (point.x > size.x || point.y > size.y) return false;
    return true;
}


struct AudioInToTransmitter : dsp::Processor<dsp::stereo_t, dsp::complex_t> {

    std::string submode;
    dsp::convert::StereoToMono s2m;
    dsp::convert::RealToComplex r2c;
    dsp::channel::FrequencyXlator xlator1;
    dsp::channel::FrequencyXlator xlator2;
    dsp::tap<dsp::complex_t> ftaps;
    dsp::filter::FIR<dsp::complex_t, dsp::complex_t> filter;
    dsp::loop::AGC<float> audioAgc;
    int micGain = 0;
    int sampleCount;
    int tuneFrequency = 0;

    AudioInToTransmitter(dsp::stream<dsp::stereo_t>* in) {
        init(in);
        totalRead = 0;
    }

    int totalRead;

    void setSubmode(const std::string &submode) {
        if (this->submode != submode) {
            this->submode = submode;
        }
    }

    void setMicGain(int micGain) {
        this->micGain = micGain;
    }

    void start() override {
        flog::info("AudioInToTransmitter: Start");
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
        dsp::Processor<dsp::stereo_t, dsp::complex_t>::start();
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
        if (tuneFrequency != 0) {
            int period = 48000 / tuneFrequency;
            auto thisStart = sampleCount % period;
            for(auto q=0; q<rd; q++) {
                auto angle = (thisStart + q) / period * 2 * M_PI;
                out.writeBuf[q].re = sin(angle);
                out.writeBuf[q].im = cos(angle);
            }
        } else {
            s2m.process(rd, this->_in->readBuf, s2m.out.writeBuf);
            if (this->postprocess) {
                r2c.process(rd, s2m.out.writeBuf, r2c.out.writeBuf);
                xlator1.process(rd, r2c.out.writeBuf, xlator1.out.writeBuf);
                filter.process(rd, xlator1.out.writeBuf, filter.out.writeBuf);
                xlator2.process(rd, filter.out.writeBuf, out.writeBuf);
                auto micAmp = pow(10, micGain / 10.0);
                for (int i = 0; i < rd; i++) {
                    out.writeBuf[i] *= micAmp;
                }
            }
            else {
                r2c.process(rd, s2m.out.writeBuf, out.writeBuf);
            }
        }
//
//        audioAgc.process(rd, s2m.out.writeBuf, audioAgc.out.writeBuf);

        sampleCount+=rd;
        this->_in->flush();
        if (!out.swap(rd)) {
            flog::info("Does not write to output stream", totalRead);
            return -1;
        }
        return rd;
    };

    bool postprocess = true;
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
    std::atomic<int> receivedSamplesCount;


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
        receivedSamplesCount.fetch_add(rd);
        int offset = inputData.size();
        inputData.resize(inputData.size() + rd);
        std::copy(_in->readBuf + 0, _in->readBuf + rd, inputData.begin()+offset);
        if (inputData.size() >= windowSize) {
            for(int q=0; q<windowSize; q++) {
                (*inputArray)[q] = dsp::complex_t{ inputData[q].l, 0 };
            }
//            static int counter = 0;
//            if (counter++ % 30 == 0) {
//                auto ib = (float*)(*inputArray).data();
//                char buf[1000];
//                sprintf(buf, "input: %f %f %f %f %f %f %f %f", ib[0], ib[1], ib[2], ib[3], ib[4], ib[5], ib[6], ib[7]);
//                std::string sbuf = buf;
//                flog::info("in qsopanel: {}", sbuf);
//            }
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

struct CWPanel {

    bool leftPressed;
    bool rightPressed;

    long long currentTime = 0;

    const int DOT = 1;
    const int DA = 2;
    int dot = 4;
    int state = 0; // 0 = nothing 1 = dot 2 = das
    int stateTime = 0;

    bool daWanted = false;
    bool dotWanted = false;


    void draw() {
        ImVec2 p1 = ImGui::GetCursorScreenPos();
        ImVec2 canvas_size = ImGui::GetContentRegionAvail();
        ImGui::GetWindowDrawList()->AddRectFilled(p1, p1 + canvas_size, IM_COL32(40,40,40,255));
        ImGui::Text("CW Panel here");

        auto windowPos = ImGui::GetWindowPos();
        auto widgetPos = windowPos + ImGui::GetCursorPos();
        auto avail =  ImGui::GetContentRegionAvail();
        auto window = ImGui::GetCurrentWindow();

#ifndef __ANDROID__
        auto mouseCoord = ImVec2(ImGui::GetMousePos().x - widgetPos.x, ImGui::GetMousePos().y - widgetPos.y);
        bool inside = withinRect(avail, mouseCoord);
        bool newLeftPressed = inside && ImGui::IsMouseDown(1);     // inverted: mouse faces the palm with eyes, two fingers (big and pointer) lay on the mouse like on paddle.
        bool newRightPressed = inside && ImGui::IsMouseDown(0);
#else
        static int fingers[10];     // 0 = nothing, 1 = left, 2 = right
        bool newLeftPressed = false;
        bool newRightPressed = false;
        for(int i=0; i<10; i++) {
            if (ImGui::IsFingerDown(i) && fingers[i] == 0) {
                auto coord = ImVec2(ImGui::GetFingerPos(i).x - widgetPos.x, ImGui::GetFingerPos(i).y - widgetPos.y);
                bool inside = withinRect(avail, coord);
                if (inside) {
                    auto halfSize =  avail;
                    halfSize.x /= 2;
                    bool left = withinRect(halfSize, coord);
                    if (left) {
                        fingers[i] = 1;
                    } else {
                        fingers[i] = 2;
                    }
                }
                // just pressed
            }
            if (!ImGui::IsFingerDown(i) && fingers[i] != 0) {
                fingers[i] = 0;
                // just released
            }
            if (fingers[i] == 1) newLeftPressed = true;
            if (fingers[i] == 2) newRightPressed = true;
        }
#endif
        if (newLeftPressed != leftPressed) {
            leftPressed = newLeftPressed;
            if (newLeftPressed) {
                sendClickSound();
            }
        }
        if (newRightPressed != rightPressed) {
            rightPressed = newRightPressed;
            if (rightPressed) {
                sendClickSound();
            }
        }

        bool sendDa = leftPressed || daWanted;
        bool sendDot = rightPressed || dotWanted;

        if (sendDa) daWanted = true;
        if (sendDot) dotWanted = true;

        stateTime++;
        if (state == DOT && stateTime == dot * 2     // dot tone, dot silence
            || state == DA && stateTime == dot * 4
            || state == 0
            ) { // da 3*tone, da silence
            // time to make a decision
            if (sendDot && sendDa) {
                if (state == DA) {
                    state = DOT;
                } else {
                    state = DA;
                }
                // nothing
            } else if (sendDot) {
                dotWanted = false;
                state = DOT;
            } else if (sendDa) {
                daWanted = false;
                state = DA;
            } else {
                state = 0;
            }
            stateTime = 0;
        }
        if (stateTime == 0 && state != 0) {
            // start any tone
            flog::info("Set Tone Enabled: true, time={}, ctm={}", currentTime, currentTimeMillis());
            setToneEnabled(true);
        }
        if (state == DOT && stateTime == dot || state == DA && stateTime == 3*dot) {
            // stop tone at specified time
            flog::info("Set Tone Enabled: OFF , time={}, ctm={}", currentTime, currentTimeMillis());
            setToneEnabled(false);
        }
        if (state == DA) {
            daWanted = false;
        }
        if (state == DOT) {
            dotWanted = false;
        }

        if (clickSendTime == currentTime) {
            sigpath::sinkManager.toneGenerator.store(2);
        } else if (toneEnabled) {
            sigpath::sinkManager.toneGenerator.store(1);
        } else {
            sigpath::sinkManager.toneGenerator.store(0);
        }
        currentTime++;
    }


    bool toneEnabled = false;
    long long clickSendTime = 0;
    void setToneEnabled(bool enabled) {
        toneEnabled = enabled;
    }

    void sendClickSound() {
        clickSendTime = currentTime;
    }

};

struct SimpleRecorder {
    enum {
        RECORDING_IDLE,
        RECORDING_STARTED,
        PLAYING_STARTED,
    } mode;
    dsp::stream<dsp::stereo_t> audioIn;
    bool recording;
    bool playing;
    std::vector<float> data;
    void startRecording() {
        if (mode == RECORDING_IDLE) {
            data.clear();
            mode = RECORDING_STARTED;
            sigpath::sinkManager.defaultInputAudio.bindStream(&audioIn);
            std::thread x([this] {
                dsp::convert::StereoToMono s2m;
                while (true) {
                    auto rd = audioIn.read();
                    if (rd < 0) {
                        flog::info("audioInToFFT->out.read() causes loop break, rd={0}", rd);
                        break;
                    }
                    s2m.process(rd, audioIn.readBuf, s2m.out.writeBuf);
                    std::copy(s2m.out.writeBuf, s2m.out.writeBuf + rd, data.end());
                }
            });
            x.detach();
        }
    }
    void startPlaying() {
        if (data.size() > 0) {
            mode = PLAYING_STARTED;
            std::thread x([this] {
                dsp::convert::StereoToMono s2m;
                while (true) {
                    auto rd = audioIn.read();
                    if (rd < 0) {
                        flog::info("audioInToFFT->out.read() causes loop break, rd={0}", rd);
                        break;
                    }
                    s2m.process(rd, audioIn.readBuf, s2m.out.writeBuf);
                    std::copy(s2m.out.writeBuf, s2m.out.writeBuf + rd, data.end());
                }
            });
            x.detach();
        }
    }
    void stop() {
        switch(mode) {
        case RECORDING_STARTED:
            sigpath::sinkManager.defaultInputAudio.unbindStream(&audioIn);
            audioIn.stopReader();
            mode = RECORDING_IDLE;
            break;
        default:
            break;
        }
    }
};

struct ConfigPanel {
    SimpleRecorder recorder;
    bool recording = false;
    void draw();
};

struct QSOPanel {
    void startSoundPipeline();
    void stopSoundPipeline();
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
    void handleTxButton(bool tx, bool tune);
    float currentFreq;
    int txGain;
    float micGain = 0;    // in db
    bool enablePA;
    bool transmitting;
    void setModeSubmode(const std::string& mode, const std::string& submode);
    std::string submode;
    std::string mode;
    EventHandler<float> maxSignalHandler;
    std::atomic<float> maxSignal;
    bool postprocess = true;
    int triggerTXOffEvent = 0;
    std::vector<float> lastSWR, lastForward, lastReflected;
    void drawHistogram();
};



static bool withinRadius(ImVec2 center, float radius, ImVec2 point) {
    if (isnan(point.x)) {
        return false;
    }
    float dx = point.x - center.x;
    float dy = point.y - center.y;
    return dx*dx+dy*dy < radius*radius;
}

bool MobileButton::isLongPress() {
    bool retval = this->currentlyPressedTime != 0 && this->currentlyPressed && currentTimeMillis() > this->currentlyPressedTime + 1000;
    if (retval) {
        this->currentlyPressedTime = 0; // prevent sending click event
    }
    return retval;
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
    ImGuiContext& g = *GImGui;
    if (pressed) {
        const float t = g.IO.MouseDownDuration[0];
        if (t < 0.2) {
            if (isnan(this->pressPoint.x)) {
                this->pressPoint = mouseCoord;
                if (withinRect(avail, this->pressPoint)) {
                    this->currentlyPressed = true; // not cleared when finger away of button
                    this->currentlyPressedTime = currentTimeMillis();
                }
            }
        }
    } else {
        this->currentlyPressed = false;
        if (startedInside && withinRect(avail, mouseCoord)) {
            // released inside
            if (currentlyPressedTime != 0) {    // handle release, longpress not happened
                retval = true;
            }
        }
        currentlyPressedTime = 0;
        this->pressPoint = ImVec2(nan(""), nan(""));
    }

    if (pressed && startedInside && withinRect(avail, mouseCoord)) {
        if (this->currentlyPressedTime != 0) {
            bg0 = ImGui::ColorConvertFloat4ToU32(ImVec4(1.0f, 1.0f, 0.5f, 1.0f));
        } else {
            // remove the highlight after long press
        }
    }

    window->DrawList->AddRectFilled(windowPos, windowPos + avail, bg0, avail.y / 10.0);


    
//    auto ts = ImGui::CalcTextSize(this->upperText.c_str());
//    ImGui::SetCursorPos((ImVec2{avail.x, 0} - ImVec2{ts.x, 0}) / 2);
//    ImGui::Text("%s", this->upperText.c_str());

    const char* buttonTextStr = this->buttonText.c_str();
//    if (this->buttonText.size() == 1) {
//        ImGui::PushFont(style::bigFont);
//    }
    auto ts = ImGui::CalcTextSize(buttonTextStr);
    ImGui::SetCursorPos((avail - ImVec2{ts.x, ts.y}) / 2);
    ImGui::Text("%s", buttonTextStr);
//    if (this->buttonText.size() == 1) {
//        ImGui::PopFont();
//    }
    ImGui::PopFont();

    return retval;
}

double TheEncoder::draw(ImGui::WaterfallVFO* vfo) {
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
    float defaultButtonsWidth = 300;
    float buttonsWidth = defaultButtonsWidth;
    float statusHeight = 100;
    switch(qsoMode) {
        case VIEW_QSO:
            buttonsWidth = 600;
            break;
        case VIEW_CONFIG:
            buttonsWidth = 1200;
            break;
        default:
            break;
    }

    const ImVec2 waterfallRegion = ImVec2(ImGui::GetContentRegionAvail().x - encoderWidth - buttonsWidth, ImGui::GetContentRegionAvail().y - statusHeight);

    lockWaterfallControls = showMenu || (qsoMode != VIEW_DEFAULT && modeToggle.upperText == "CW");
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
    ImGui::PushFont(style::mediumFont);
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
        auto rectRegion = menuRegion;
        rectRegion.y += 10000;      // menu is scrolling, rect somehow scrolls with it, so must be big enough to be always visible
        window->DrawList->AddRectFilled(widgetPos, widgetPos + rectRegion, bg);

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
            float od = offsetDelta * 100;
            float sign = od < 0 ? -1 : 1;
            if (fabs(od) < 2) {
                od = 0.5 * sign;
            } else {
                od = fabs(od) - 1;    // 4 will become 1
                od = pow(od, 1.4) * sign;
//                od *= 5;
            }
            flog::info("od={} encoder.currentFrequency={}", od, encoder.currentFrequency);
            encoder.currentFrequency -= od;
            auto cf = ((int)(encoder.currentFrequency / 10)) * 10.0;
            tuner::tune(tuningMode, gui::waterfall.selectedVFO, cf);
        }
    }
    auto currentFreq = (vfo != NULL) ? (vfo->generalOffset + gui::waterfall.getCenterFrequency()) : gui::waterfall.getCenterFrequency();
    this->autoDetectBand((int)currentFreq);
    ImGui::EndChild(); // encoder


    ImGui::SetCursorPos(cornerPos + ImVec2{waterfallRegion.x, 0});
    auto vertPadding = ImGui::GetStyle().WindowPadding.y;


    MobileButton *buttonsDefault[] = {&this->modeToggle, &this->bandUp, &this->bandDown, &this->submodeToggle, &this->zoomToggle, &this->qsoButton };
    MobileButton *buttonsQso[] = {&this->qsoButton, &this->txButton, &this->softTune };
    MobileButton *buttonsConfig[] = {&this->exitConfig };
    auto nButtonsQso = (int)((sizeof(buttonsQso) / sizeof(buttonsQso[0])));
    auto nButtonsDefault = (int)((sizeof(buttonsDefault) / sizeof(buttonsDefault[0])));
    auto nButtonsConfig = (int)((sizeof(buttonsConfig) / sizeof(buttonsConfig[0])));
    auto buttonsSpaceY = ImGui::GetContentRegionAvail().y;
    switch(this->qsoMode) {
        case VIEW_QSO:
            qsoButton.buttonText = "End QSO";
        case VIEW_DEFAULT:
            qsoButton.buttonText = "QSO";
        default:
            break;
    }
    const ImVec2 buttonsRegion = ImVec2(buttonsWidth, buttonsSpaceY);
    ImGui::BeginChildEx("Buttons", ImGui::GetID("sdrpp_mobile_buttons"), buttonsRegion, false, ImGuiWindowFlags_NoScrollbar);

    auto beforePanel = ImGui::GetCursorPos();
    if (this->qsoMode == VIEW_CONFIG) {
            ImGui::BeginChildEx("Config", ImGui::GetID("sdrpp_mobconffig"), ImVec2(buttonsWidth, ImGui::GetContentRegionAvail().y), false, 0);
            configPanel->draw();
            ImGui::EndChild(); // buttons
    }
    if (this->qsoMode == VIEW_QSO) {
        ImGui::BeginChildEx("QSO", ImGui::GetID("sdrpp_qso"), ImVec2(buttonsWidth, ImGui::GetContentRegionAvail().y), false, 0);
        qsoPanel->draw(currentFreq, vfo);
        auto afterQSOPanel = ImGui::GetCursorPos();
        ImGui::EndChild(); // buttons

//        ImGui::SetCursorPos(ImVec2{beforeQSOPanel.x, beforeQSOPanel.y + afterQSOPanel.y});
//        buttonsSpaceY -= afterQSOPanel.y;
    }
    ImGui::SetCursorPos(beforePanel);


    MobileButton **buttons;
    int NBUTTONS;
    switch(this->qsoMode) {
        case VIEW_QSO:
            buttons = buttonsQso;
            NBUTTONS = nButtonsQso;
            break;
        case VIEW_DEFAULT:
            NBUTTONS = nButtonsDefault;
            buttons = buttonsDefault;
            break;
        case VIEW_CONFIG:
            NBUTTONS = nButtonsConfig;
            buttons = buttonsConfig;
            break;
    }
    MobileButton *pressedButton = nullptr;
    auto buttonsStart = ImGui::GetCursorPos();
    for (auto b=0; b<NBUTTONS; b++) {
        char chi[100];
        sprintf(chi, "mob_button_%d", b);
        const ImVec2 childRegion = ImVec2(defaultButtonsWidth, buttonsSpaceY / nButtonsDefault - vertPadding);
        if (this->qsoMode == VIEW_DEFAULT) {
            ImGui::SetCursorPos(buttonsStart + ImVec2{ 0, buttonsSpaceY - (nButtonsDefault - b) * (childRegion.y + vertPadding) });
        } else {
            ImGui::SetCursorPos(buttonsStart + ImVec2{buttonsWidth - defaultButtonsWidth, buttonsSpaceY - (b + 1) * (childRegion.y + vertPadding) });
        }
        ImGui::BeginChildEx(chi, ImGui::GetID(chi), childRegion, false, 0);
        auto pressed = buttons[b]->draw();
        ImGui::EndChild();
        if (pressed) {
            pressedButton = buttons[b];
        }
    }
    ImGui::EndChild(); // buttons

    if (qsoMode == VIEW_QSO && modeToggle.upperText == "CW") {
        ImGui::SetCursorPos(cornerPos + ImVec2(waterfallRegion.x / 4, waterfallRegion.y / 2));
        ImVec2 childRegion(waterfallRegion.x/2, waterfallRegion.y / 2);
        ImGui::BeginChildEx("cwbuttons", ImGui::GetID("cwbuttons"), childRegion, true, 0);
        cwPanel->draw();
        ImGui::EndChild(); // buttons
    }

    ImGui::End();

    auto makeZoom = [&](int selectedIndex) {
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
    };
    if (this->zoomToggle.isLongPress()) {
        makeZoom(0);
    }
    if (this->qsoButton.isLongPress()) {
        if (!this->qsoPanel->audioInToFFT) {
            this->qsoPanel->startSoundPipeline();
        }
        this->qsoMode = VIEW_CONFIG;
    }
    if (pressedButton == &this->exitConfig) {
        this->qsoMode = VIEW_DEFAULT;
        this->qsoPanel->stopSoundPipeline();
    }
    if (pressedButton == &this->zoomToggle) {
        int selectedIndex = -1;
        for(auto q = 0; q<zooms.size(); q++) {
            if (zooms[q].first == zoomToggle.upperText) {
                selectedIndex = q;
                break;
            }
        }
        selectedIndex = (selectedIndex + 1)%zooms.size();
        makeZoom(selectedIndex);
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
        this->qsoMode = this->qsoMode == VIEW_DEFAULT ? VIEW_QSO : VIEW_DEFAULT;
        if (this->qsoMode == VIEW_QSO) {
            qsoPanel->startSoundPipeline();
        } else {
            qsoPanel->stopSoundPipeline();
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
    qsoPanel->setModeSubmode(modeToggle.upperText, submodeToggle.upperText);
    if (sigpath::transmitter && qsoPanel->triggerTXOffEvent < 0) {        // transiver exists, and TX is not in handing off state
        if (pressedButton == &this->softTune) {
            if (qsoPanel->audioInToTransmitter) {
                // tuning, need to stop
                qsoPanel->handleTxButton(false, true);
#define SOFT_TUNE_LABEL "SoftTune"
                pressedButton->buttonText = SOFT_TUNE_LABEL;
            }
            else {
                // need to start
                qsoPanel->handleTxButton(true, true);
                pressedButton->buttonText = "Tuning..";
            }
        }
        if (txButton.currentlyPressed && !qsoPanel->audioInToTransmitter || !txButton.currentlyPressed && qsoPanel->audioInToTransmitter && qsoPanel->audioInToTransmitter->tuneFrequency == 0) {
            // button is pressed ok to send, or button is released and audioInToTransmitter running and it is not in tune mode
            qsoPanel->handleTxButton(this->txButton.currentlyPressed, false);
            //
        }
    } else if (!sigpath::transmitter) {
        if (txButton.currentlyPressed) {
            sigpath::sinkManager.toneGenerator.store(1);
        } else {
            sigpath::sinkManager.toneGenerator.store(0);
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
                         txButton("", "TX"),
                         exitConfig("", "OK"),
                         softTune("", SOFT_TUNE_LABEL)
{
    qsoPanel = std::make_shared<QSOPanel>();
    configPanel = std::make_shared<ConfigPanel>();
    cwPanel = std::make_shared<CWPanel>();
}


void QSOPanel::startSoundPipeline() {
    readSamples = 0;
    sigpath::sinkManager.defaultInputAudio.bindStream(&audioIn);
    audioInToFFT = std::make_shared<AudioInToFFT>(&audioIn);
    audioInToFFT->start();
    sigpath::averageTxSignalLevel.bindHandler(&maxSignalHandler);
    maxSignalHandler.ctx = this;
    maxSignalHandler.handler = [](float maxSignal, void *ctx) {
        auto _this = (QSOPanel*)ctx;
        _this->maxSignal.store(maxSignal);
    };
    std::thread x([this]{
        while (true) {
            auto rd = audioInToFFT->out.read();
            if (rd < 0) {
                flog::info("audioInToFFT->out.read() causes loop break, rd={0}", rd);
                break;
            }
//            flog::info("audioInToFFT->out.read() exited, data={0} {1} {2} {3} {4} {5} {6} {7}",
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


void QSOPanel::setModeSubmode(const std::string &mode, const std::string &submode) {
    if (this->submode != submode) {
        this->submode = submode;
    }
    if (this->mode != mode) {
        this->mode = mode;
    }
}

void QSOPanel::handleTxButton(bool tx, bool tune) {
    if (tx) {
        if (!this->transmitting) {
            sigpath::txState.emit(tx);
            this->transmitting = true;
            if (sigpath::transmitter) {
                sigpath::sinkManager.defaultInputAudio.bindStream(&audioTowardsTransmitter);
                audioInToTransmitter = std::make_shared<AudioInToTransmitter>(&audioTowardsTransmitter);
                audioInToTransmitter->setSubmode(submode);
                audioInToTransmitter->setMicGain(micGain);
                audioInToTransmitter->postprocess = postprocess;
                audioInToTransmitter->tuneFrequency = tune ? 20 : 0; // 20hz close to carrier
                audioInToTransmitter->start();
                sigpath::transmitter->setTransmitStream(&audioInToTransmitter->out);
                sigpath::transmitter->setTransmitFrequency((int)currentFreq);
                sigpath::transmitter->setTransmitStatus(true);
            }
            lastSWR.clear();
            lastForward.clear();
            lastReflected.clear();
        }
    } else {
        if (this->transmitting) {
            this->transmitting = false;
            triggerTXOffEvent = 14; // 60fps,
            if (sigpath::transmitter) {
                sigpath::transmitter->setTransmitStatus(false);
                sigpath::sinkManager.defaultInputAudio.unbindStream(&audioTowardsTransmitter);
                audioInToTransmitter->stop();
                audioInToTransmitter->out.stopReader();
            }
        }
    }
}

void QSOPanel::stopSoundPipeline() {
    sigpath::sinkManager.defaultInputAudio.unbindStream(&audioIn);
    flog::info("QSOPanel::stop. Calling audioInToFFT->stop()");
    audioInToFFT->stop();
    audioInToFFT->out.stopReader();
    flog::info("Calling audioInToFFT->reset()");
    audioInToFFT.reset();
    flog::info("Reset complete");
}

float rtmax(std::vector<float> &v) {
    if (v.empty()) {
        return 0;
    }
    while(v.size() > 20) {
        v.erase(v.begin());
    }
    auto m = v[0];
    for(int q=1; q<v.size(); q++) {
        if (v[q] > m) {
            m = v[q];
        }
    }
    return m;
}

void QSOPanel::drawHistogram() {
    if (!currentFFTBuffer || audioInToFFT->receivedSamplesCount.load() == 0) {
        ImGui::TextColored(ImVec4(1.0f, 0, 0, 1.0f), "%s", "No microphone! Please");
        ImGui::TextColored(ImVec4(1.0f, 0, 0, 1.0f), "%s", "allow mic access in");
        ImGui::TextColored(ImVec4(1.0f, 0, 0, 1.0f), "%s", "the app permissions!");
        ImGui::TextColored(ImVec4(1.0f, 0, 0, 1.0f), "%s", "(and then restart)");
    } else {
        if (submode == "LSB"|| submode == "USB") {
            auto mx0 = npmax(currentFFTBuffer);
            auto data = npsqrt(currentFFTBuffer);
            auto mx = npmax(data);
            // only 1/4 of spectrum
            ImVec2 space = ImGui::GetContentRegionAvail();
            space.y = 60;  // fft height
            ImGui::PlotHistogram("##fft", data->data(), currentFFTBuffer->size() / 4, 0, NULL, 0,
                                 mx,
                                 space);
            if (ImGui::SliderFloat("##_radio_mic_gain_", &this->micGain, 0, +22, "%.1f dB")) {
                //
            }
            ImGui::SameLine();
            ImGui::Text("Mic:%.3f", mx0);
        }
        if (submode == "CWU" || submode == "CWL") {
            ImGui::Text("CW Mode - use paddle");
        }
    }
}

void QSOPanel::draw(float currentFreq, ImGui::WaterfallVFO* pVfo) {
    this->currentFreq = currentFreq;
    currentFFTBufferMutex.lock();
    this->drawHistogram();
    ImGui::Text("Max TX: %f", this->maxSignal.load());
    if (sigpath::transmitter) {
        ImGui::Text("TRX Qsz: %d", (int)sigpath::transmitter->getFillLevel());
        ImGui::SameLine();
        if (ImGui::Checkbox("PostPro", &this->postprocess)) {

        }
        float swr = sigpath::transmitter->getTransmitSWR();
        if (swr >= 9.9) swr = 9.9; // just not to jump much
        lastSWR.emplace_back(swr);
        lastForward.emplace_back(sigpath::transmitter->getTransmitPower());
        lastReflected.emplace_back(sigpath::transmitter->getReflectedPower());
        ImGui::Text("SWR:%.1f", rtmax(lastSWR));
        ImGui::SameLine();
        ImGui::Text("FWD:%.1f", rtmax(lastForward));  // below 10w will - not jump.
        ImGui::SameLine();
        ImGui::Text("REF:%.1f", rtmax(lastReflected));
        if (ImGui::Checkbox("PA", &this->enablePA)) {
            sigpath::transmitter->setPAEnabled(this->enablePA);
        }
        ImGui::SameLine();
        ImGui::LeftLabel("TX:");
        ImGui::SameLine();
        if (ImGui::SliderInt("##_radio_tx_gain_", &this->txGain, 0, 255)) {
            sigpath::transmitter->setTransmitGain(this->txGain);
        }
    } else {
        ImGui::TextColored(ImVec4(1.0f, 0, 0, 1.0f), "%s", "Transmitter not found");
        ImGui::TextColored(ImVec4(1.0f, 0, 0, 1.0f), "%s", "Select a device");
        ImGui::TextColored(ImVec4(1.0f, 0, 0, 1.0f), "%s", "(e.g. HL2 Source)");
    }
    currentFFTBufferMutex.unlock();

    triggerTXOffEvent--;
    if (!triggerTXOffEvent) { // zero cross
        audioInToTransmitter.reset();
        sigpath::txState.emit(false);
    }

}

void ConfigPanel::draw() {
    gui::mainWindow.qsoPanel->currentFFTBufferMutex.lock();
    gui::mainWindow.qsoPanel->drawHistogram();
    gui::mainWindow.qsoPanel->currentFFTBufferMutex.unlock();
    if (ImGui::Button(recording ? "Stop Rec" : "Record")) {
        recording = !recording;
        if (recording) {
            recorder.startRecording();
        } else {
            recorder.stop();
        }
    }
}