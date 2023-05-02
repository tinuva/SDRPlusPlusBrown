#define IMGUI_DEFINE_MATH_OPERATORS
#define _USE_MATH_DEFINES // NOLINT(bugprone-reserved-identifier)
#include <gui/mobile_main_window.h>
#include <gui/gui.h>
#include "imgui_internal.h"
#include "imgui.h"
#include <cstdio>
#include <thread>
#include <algorithm>
#include <gui/widgets/waterfall.h>
#include <gui/icons.h>
#include <gui/widgets/bandplan.h>
#include <gui/widgets/finger_button.h>
#include <gui/style.h>
#include <gui/menus/display.h>
#include <gui/menus/theme.h>
#include <filesystem>
#include <gui/tuner.h>
#include <gui/widgets/small_waterfall.h>
#include "../../decoder_modules/radio/src/radio_module.h"
#include "../../misc_modules/noise_reduction_logmmse/src/arrays.h"
#include "../../misc_modules/noise_reduction_logmmse/src/af_nr.h"
#include "dsp/convert/stereo_to_mono.h"
#include "dsp/convert/real_to_complex.h"
#include "dsp/convert/complex_to_real.h"
#include "dsp/channel/frequency_xlator.h"
#include "ctm.h"
#include <cmath>
#include <utility>

using namespace ::dsp::arrays;

float trxAudioSampleRate = 48000.0;

const int nSmallWheelFunctions = 3;

const char* smallWheelFunctionName(int n) {
    switch (n % nSmallWheelFunctions) {
    case 0:
        return "Zoom/";
    case 1:
        return "Brightness/";
    case 2:
        return "Volume/";
    default:
        return "WTF";
    }
}

template <typename X>
void setConfig(const std::string& key, X value) {
    core::configManager.acquire();
    flog::info("Setting core config: {} = {}", key.c_str(), std::to_string(value).c_str());
    core::configManager.conf[key] = value;
    core::configManager.release(true);
}

template <typename X>
void getConfig(const std::string& key, X& value) {
    core::configManager.acquire();
    if (core::configManager.conf.find(key) != core::configManager.conf.end()) {
        value = core::configManager.conf[key];
    }
    core::configManager.release(false);
}

struct Decibelometer {
    float maxSignalPeak = -1000.0;
    long long maxSignalPeakTime = 0.0;
    std::vector<float> lastMeasures;
    void addSamples(dsp::stereo_t* samples, int count) {
        if (count > 0) {
            float summ = 0;
            for (int i = 0; i < count; i++) {
                summ += samples[i].l * samples[i].l;
            }
            summ = sqrt(summ / count);
            lastMeasures.emplace_back(20 * log10(summ));
            if (lastMeasures.size() > 100) {
                lastMeasures.erase(lastMeasures.begin());
            }
            updateMax();
        }
    }

    void addSamples(dsp::complex_t* samples, int count) {
        if (count > 0) {
            float summ = 0;
            for (int i = 0; i < count; i++) {
                summ += samples[i].amplitude() * samples[i].amplitude();
            }
            summ = sqrt(summ / count);
            lastMeasures.emplace_back(20 * log10(summ));
            if (lastMeasures.size() > 100) {
                lastMeasures.erase(lastMeasures.begin());
            }
            updateMax();
        }
    }

    float getAvg(int len) {
        if (len > lastMeasures.size()) {
            len = lastMeasures.size();
        }
        float sum = 0;
        for (int i = 0; i < len; i++) {
            sum += lastMeasures[lastMeasures.size() - 1 - i];
        }
        return sum / len;
    }

    float getMax(int len) {
        if (len > lastMeasures.size()) {
            len = lastMeasures.size();
        }
        float maxx = -1000;
        for (int i = 0; i < len; i++) {
            maxx = std::max<float>(maxx, lastMeasures[lastMeasures.size() - 1 - i]);
        }
        return maxx;
    }

    void addSamples(float* samples, int count) {
        float summ = 0;
        for (int i = 0; i < count; i++) {
            summ += samples[i] * samples[i];
        }
        summ = sqrt(summ / count);
        lastMeasures.emplace_back(20 * log10(summ));
        if (lastMeasures.size() > 100) {
            lastMeasures.erase(lastMeasures.begin());
        }
        updateMax();
    }

    float getPeak() {
        return maxSignalPeak;
    }

    void updateMax() {
        auto mx = *lastMeasures.end();
        if (mx > maxSignalPeak || maxSignalPeakTime < currentTimeMillis() - 500) {
            maxSignalPeak = mx;
            maxSignalPeakTime = currentTimeMillis();
            ;
        }
    }
};


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
    int micGain = 0;
    int sampleCount = 0;
    int tuneFrequency = 0;
    float bandwidth = 4000.0;


    AudioInToTransmitter(dsp::stream<dsp::stereo_t>* in) {
        init(in);
        totalRead = 0;
    }

    int totalRead;

    void setSubmode(const std::string& _submode) {
        if (this->submode != _submode) {
            this->submode = _submode;
        }
    }

    void setMicGain(int _micGain) {
        this->micGain = _micGain;
    }

    void start() override {
        flog::info("AudioInToTransmitter: Start");
        if (this->submode == "LSB") {
            xlator1.init(nullptr, -(bandwidth) / 2, trxAudioSampleRate);
            xlator2.init(nullptr, bandwidth / 2, trxAudioSampleRate);
        }
        else {
            xlator1.init(nullptr, (bandwidth) / 2, trxAudioSampleRate);
            xlator2.init(nullptr, -bandwidth / 2, trxAudioSampleRate);
        }
        ftaps = dsp::taps::lowPass0<dsp::complex_t>(bandwidth / 2, bandwidth / 2 * 0.1, trxAudioSampleRate);
        filter.init(nullptr, ftaps);

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
            // generate sine wave for soft tune
            int period = trxAudioSampleRate / tuneFrequency;
            auto thisStart = sampleCount % period;
            for (auto q = 0; q < rd; q++) {
                auto angle = (thisStart + q) / (double)period * 2 * M_PI;
                out.writeBuf[q].re = (float)sin(angle);
                out.writeBuf[q].im = (float)cos(angle);
            }
        }
        else {
            dsp::convert::StereoToMono::process(rd, this->_in->readBuf, s2m.out.writeBuf);
            int gain = 4;
            if (this->postprocess) {
                dsp::convert::RealToComplex::process(rd, s2m.out.writeBuf, r2c.out.writeBuf);
                xlator1.process(rd, r2c.out.writeBuf, xlator1.out.writeBuf);
                filter.process(rd, xlator1.out.writeBuf, filter.out.writeBuf);
                xlator2.process(rd, filter.out.writeBuf, out.writeBuf);
                for (int q = 0; q < rd; q++) {
                    out.writeBuf[q] *= gain;
                }
            }
            else {
                dsp::convert::RealToComplex::process(rd, s2m.out.writeBuf, out.writeBuf);
            }
        }
        sampleCount += rd;
        this->_in->flush();
        if (!out.swap(rd)) {
            flog::info("Does not write to output stream", totalRead);
            return -1;
        }
        return rd;
    };

    bool postprocess = true;
};

struct AudioFFTForDisplay : dsp::Processor<dsp::stereo_t, dsp::complex_t> {

    template <class X>
    using Arg = std::shared_ptr<X>;

    using base_type = dsp::Processor<dsp::stereo_t, dsp::complex_t>;


    std::vector<dsp::stereo_t> inputData;
    float& FREQUENCY = trxAudioSampleRate;
    const int FRAMERATE = 60;
    Arg<fftwPlan> plan;
    const int windowSize = FREQUENCY / FRAMERATE;
    ComplexArray inputArray;
    FloatArray hanningWin;
    std::atomic<int> receivedSamplesCount = 0;
    dsp::stream<dsp::stereo_t> strm;
    dsp::routing::Splitter<dsp::stereo_t>* in;


    explicit AudioFFTForDisplay(dsp::routing::Splitter<dsp::stereo_t>* in) {
        init(&strm);
        this->in = in;
        inputData.reserve(4096);
        plan = dsp::arrays::allocateFFTWPlan(false, windowSize);
        inputArray = npzeros_c(windowSize);
        hanningWin = nphanning(windowSize);
        hanningWin = div(mul(hanningWin, (float)windowSize), npsum(hanningWin));
    }

    ~AudioFFTForDisplay() override {
        if (running) {
            stop();
        }
    }

    //    bool started

    void start() override {
        if (!running) {
            base_type::start();
            in->bindStream(&strm);
        }
    }

    void stop() override {
        if (running) {
            in->unbindStream(&strm);
            base_type::stop();
        }
    }

    int run() override {
        //        flog::info("AudioFFTForDisplay.read...()");
        int rd = 0;
        if (inputData.size() < windowSize) {
            rd = this->strm.read();
            //            flog::info("AudioFFTForDisplay.read rd={}", rd);
            if (rd < 0) {
                return rd;
            }
        }
        receivedSamplesCount.fetch_add(rd);
        auto offset = inputData.size();
        inputData.resize(inputData.size() + rd);
        std::copy(strm.readBuf + 0, strm.readBuf + rd, inputData.begin() + offset);
        if (inputData.size() >= windowSize) {
            for (int q = 0; q < windowSize; q++) {
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
            this->out.swap((int)result->size());
            //            flog::info("input data size: {} erase {}", inputData.size(), result->size());
            inputData.erase(inputData.begin(), inputData.begin() + windowSize);
        }
        this->strm.flush();
        return rd;
    };
};

struct CWPanel {

    bool leftPressed = false;
    bool rightPressed = false;

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
        ImGui::GetWindowDrawList()->AddRectFilled(p1, p1 + canvas_size, IM_COL32(40, 40, 40, 255));
        ImGui::Text("CW Panel here");

        auto windowPos = ImGui::GetWindowPos();
        auto widgetPos = windowPos + ImGui::GetCursorPos();
        auto avail = ImGui::GetContentRegionAvail();
        //        auto window = ImGui::GetCurrentWindow();

#ifndef __ANDROID__
        auto mouseCoord = ImVec2(ImGui::GetMousePos().x - widgetPos.x, ImGui::GetMousePos().y - widgetPos.y);
        bool inside = withinRect(avail, mouseCoord);
        bool newLeftPressed = inside && ImGui::IsMouseDown(1); // inverted: mouse faces the palm with eyes, two fingers (big and pointer) lay on the mouse like on paddle.
        bool newRightPressed = inside && ImGui::IsMouseDown(0);
#else
        static int fingers[10]; // 0 = nothing, 1 = left, 2 = right
        bool newLeftPressed = false;
        bool newRightPressed = false;
        for (int i = 0; i < 10; i++) {
            if (ImGui::IsFingerDown(i) && fingers[i] == 0) {
                auto coord = ImVec2(ImGui::GetFingerPos(i).x - widgetPos.x, ImGui::GetFingerPos(i).y - widgetPos.y);
                bool inside = withinRect(avail, coord);
                if (inside) {
                    auto halfSize = avail;
                    halfSize.x /= 2;
                    bool left = withinRect(halfSize, coord);
                    if (left) {
                        fingers[i] = 1;
                    }
                    else {
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
        if (state == DOT && stateTime == dot * 2                    // dot tone, dot silence
            || state == DA && stateTime == dot * 4 || state == 0) { // da 3*tone, da silence
            // time to make a decision
            if (sendDot && sendDa) {
                if (state == DA) {
                    state = DOT;
                }
                else {
                    state = DA;
                }
                // nothing
            }
            else if (sendDot) {
                dotWanted = false;
                state = DOT;
            }
            else if (sendDa) {
                daWanted = false;
                state = DA;
            }
            else {
                state = 0;
            }
            stateTime = 0;
        }
        if (stateTime == 0 && state != 0) {
            // start any tone
            flog::info("Set Tone Enabled: true, time={}, ctm={}", (int64_t)currentTime, (int64_t)currentTimeMillis());
            setToneEnabled(true);
        }
        if (state == DOT && stateTime == dot || state == DA && stateTime == 3 * dot) {
            // stop tone at specified time
            flog::info("Set Tone Enabled: OFF , time={}, ctm={}", (int64_t)currentTime, (int64_t)currentTimeMillis());
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
        }
        else if (toneEnabled) {
            sigpath::sinkManager.toneGenerator.store(1);
        }
        else {
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
    } mode = RECORDING_IDLE;
    dsp::stream<dsp::stereo_t> recorderIn;
    dsp::stream<dsp::stereo_t> recorderOut;
    std::vector<dsp::stereo_t> data;
    bool keepPlaying = true;

    dsp::routing::Splitter<dsp::stereo_t>* inputAudio;

    explicit SimpleRecorder(dsp::routing::Splitter<dsp::stereo_t>* inputAudio) : inputAudio(inputAudio) {
        recorderOut.origin = "simpleRecorder.recorderOut";
        recorderIn.origin = "simpleRecorder.recorderIn";
    }

    void startRecording() {
        if (mode == RECORDING_IDLE) {
            data.clear();
            mode = RECORDING_STARTED;
            sigpath::sinkManager.setAllMuted(true);
            recorderIn.clearReadStop();
            recorderIn.clearWriteStop();
            inputAudio->bindStream(&recorderIn);
            std::thread x([this] {
                while (true) {
                    auto rd = recorderIn.read();
                    if (rd < 0) {
                        flog::info("recorder.read() causes loop break, rd={0}", rd);
                        break;
                    }
                    auto dest = data.size();
                    data.resize(data.size() + rd);
                    std::copy(recorderIn.readBuf, recorderIn.readBuf + rd, data.begin() + (long)dest);
                    recorderIn.flush();
                    //                    flog::info("recorder.read() rd={0}, dest={1}, data.size()={2}", rd, dest, data.size());
                }
            });
            x.detach();
        }
    }

    void startPlaying() {
        if (!data.empty()) {
            auto radioName = gui::waterfall.selectedVFO;
            if (!radioName.empty()) {
                mode = PLAYING_STARTED;
                keepPlaying = true;
                std::thread x([this] {
                    flog::info("startPlaying: start");
                    dsp::routing::Merger<dsp::stereo_t>* merger = sigpath::sinkManager.getMerger(gui::waterfall.selectedVFO);
                    recorderOut.clearReadStop();
                    recorderOut.clearWriteStop();
                    merger->bindStream(-10, &recorderOut);
                    auto waitTil = (double)currentTimeMillis();
                    for (int i = 0; i < data.size() && keepPlaying; i += 1024) {
                        auto ctm = currentTimeMillis();
                        if (ctm < (long long)waitTil) {
                            std::this_thread::sleep_for(std::chrono::milliseconds((int64_t)waitTil - ctm));
                        }
                        size_t blockEnd = i + 1024;
                        if (blockEnd > data.size()) {
                            blockEnd = data.size();
                        }
                        std::copy(data.data() + i, data.data() + blockEnd, recorderOut.writeBuf);
                        waitTil += 1000 * (blockEnd - i) / trxAudioSampleRate;
                        //                        flog::info("player Swapping to {}: {0} - {1} = {}, wait til {}", audioOut.origin, blockEnd, i, blockEnd - i, (int64_t)waitTil);
                        recorderOut.swap(blockEnd - i);
                        //                        flog::info("player Swapped to {}: {} - {} = {}", audioOut.origin, blockEnd, i, blockEnd - i);
                    }
                    flog::info("startPlaying: stop");
                    merger->unbindStream(&recorderOut);
                    mode = RECORDING_IDLE;
                });
                x.detach();
            }
        }
    }
    void stop() {
        switch (mode) {
        case PLAYING_STARTED:
            keepPlaying = false;
            break;
        case RECORDING_STARTED:
            sigpath::sinkManager.setAllMuted(false);
            inputAudio->unbindStream(&recorderIn);
            recorderIn.stopReader();
            mode = RECORDING_IDLE;
            break;
        default:
            break;
        }
    }
};

struct Equalizer : public dsp::Processor<dsp::complex_t, dsp::complex_t> {

    dsp::filter::FIR<dsp::complex_t, dsp::complex_t> flt;

    std::vector<dsp::complex_t> design_bandpass_filter(float center_freq, float bandwidth, float fs) {

        float lowcut = (center_freq - bandwidth / 2) / (fs / 2);
        float highcut = (center_freq + bandwidth / 2) / (fs / 2);


        auto taps = dsp::taps::bandPass<dsp::complex_t>(center_freq - bandwidth / 2, center_freq + bandwidth / 2, bandwidth * 1.5, fs, true);
        std::vector<dsp::complex_t> filter(taps.size);

        // Apply a window function (e.g., Hamming window)
        for (int i = 0; i < taps.size; ++i) {
            taps.taps[i] *= 0.54 - 0.46 * std::cos(2 * M_PI * i / (taps.size - 1));
        }
        for (int i = 0; i < taps.size; ++i) {
            filter[i] = taps.taps[i];
        }

        dsp::taps::free(taps);

        return filter;
    }

    Equalizer() : dsp::Processor<dsp::complex_t, dsp::complex_t>() {
        std::vector<float> center_freqs = { 800, 1200, 1800, 2400 };
        std::vector<float> bandwidths = { 800, 800, 800, 800 };
        std::vector<float> gains = { 0.3, 1.0, 2.0, 2.0 };

        std::vector<dsp::complex_t> equalizer;

        for (size_t i = 0; i < center_freqs.size(); ++i) {
            auto band_filter = design_bandpass_filter(center_freqs[i], bandwidths[i], trxAudioSampleRate);
            for (int q = 0; q < band_filter.size(); ++q) {
                band_filter[q] *= gains[i];
            }
            if (i == 0) {
                equalizer = band_filter;
            }
            else {
                for (int j = 0; j < band_filter.size(); ++j) {
                    equalizer[j] += band_filter[j];
                }
            }
        }


        auto taps = dsp::taps::alloc<dsp::complex_t>(equalizer.size());
        for (int i = 0; i < equalizer.size(); ++i) {
            taps.taps[i] = equalizer[i];
        }
        taps.size = equalizer.size();
        flt.init(nullptr, taps);
    }

    void process(size_t size, dsp::complex_t* in, dsp::complex_t* out) {
        flt.process(size, in, out);
    }


    int run() override {
        return 0;
    }
};


struct FreqMaim : public dsp::Processor<dsp::complex_t, dsp::complex_t> {

    Arg<fftwPlan> forward;
    Arg<fftwPlan> backward;
    int nbuckets = 512;

    FreqMaim() : dsp::Processor<dsp::complex_t, dsp::complex_t>() {
        forward = allocateFFTWPlan(true, 512);
        backward = allocateFFTWPlan(true, 512);
    }

    std::vector<dsp::complex_t> buffer;

    void process(size_t size, dsp::complex_t* in, dsp::complex_t* out) {
        buffer.reserve(buffer.size() + size);
        buffer.insert(buffer.end(), in, in + size);
        auto ina = npzeros_c(nbuckets);
        while (buffer.size() > nbuckets) {
            for (int i = 0; i < nbuckets; ++i) {
                (*ina)[i] = buffer[i];
            }
            auto outa = npfftfft(ina, forward);
            auto rev = npfftfft(outa, backward);
            for (int i = 0; i < nbuckets; ++i) {
                out[i] = (*rev)[i];
                out[i] *= (float)nbuckets;
            }
            for (int i = nbuckets - 1; i >= 0; --i) {
                auto ndest = (int)(1.5 * i);
                if (ndest >= nbuckets) {
                    out[i].re /= 512;
                    out[i].im /= 512;
                }
                else {
                    auto amp = out[ndest].amplitude();
                    out[i].re = out[ndest].re;
                    out[i].im = 0;
                }
            }
            out += nbuckets;
            buffer.erase(buffer.begin(), buffer.begin() + nbuckets);
        }
    }


    int run() override {
        return 0;
    }
};


namespace dsp {
    struct AFNR_OMLSA_MCRA;
}

struct ConfigPanel {
    float agcAttack = 120.0f;
    float agcDecay = 0.1f;
    //    bool doNR = true;
    bool doEqualize = true;
    bool micSql = false;
    float micSqlLevel = -35;
    float compAmp = 1.0f;
    bool doFreqMaim = false;

    // audio passband
    int highPass = 120;
    int lowPass = 2700;

    dsp::convert::StereoToMono s2m;
    dsp::convert::MonoToStereo m2s;
    dsp::loop::AGC<float> agc;
    //    dsp::loop::AGC<dsp::complex_t> agc2;
    std::shared_ptr<dsp::AFNR_OMLSA_MCRA> afnr;
    Equalizer equalizer;
    FreqMaim freqMaim;
    dsp::convert::RealToComplex r2c;
    dsp::convert::ComplexToReal c2r;

    Decibelometer rawInDecibels;
    Decibelometer outDecibels;
    Decibelometer agcDecibels;
    Decibelometer equalizerDecibels;
    Decibelometer highPassDecibels;
    Decibelometer nrDecibels;

    SimpleRecorder recorder;

    float hissCut = 5000;
    float hissSize = 500;
    bool hissAdd = false;


    // hiss
    dsp::tap<dsp::complex_t> highPassForSTaps;
    dsp::filter::FIR<dsp::complex_t, dsp::complex_t> highPassForS;
    dsp::tap<dsp::complex_t> lowPassForSTaps;
    dsp::filter::FIR<dsp::complex_t, dsp::complex_t> lowPassForS;
    dsp::channel::FrequencyXlator xlatorForS;


    explicit ConfigPanel(dsp::routing::Splitter<dsp::stereo_t>* inputAudio) : recorder(inputAudio) {
        //        afnr.setProcessingBandwidth(11000);
        //        agc2.init(NULL, 1.0, agcAttack / 48000.0, agcDecay / 48000.0, 10e6, 10.0, INFINITY);
    }

    void init() {
        agc.init(NULL, 1.0, agcAttack / trxAudioSampleRate, agcDecay / trxAudioSampleRate, 10e6, 1.0, INFINITY);

        getConfig("trx_highPass", highPass);
        getConfig("trx_doEqualize", doEqualize);
        getConfig("trx_compAmp", compAmp);
        getConfig("trx_agcAttack", agcAttack);
        getConfig("trx_agcDecay", agcDecay);
        getConfig("trx_lowPass", lowPass);

        agc.setAttack(agcAttack / trxAudioSampleRate);
        agc.setDecay(agcDecay / trxAudioSampleRate);

        highPassForSTaps = dsp::taps::highPass0<dsp::complex_t>(hissCut, hissCut * 0.1, trxAudioSampleRate);
        highPassForS.init(nullptr, highPassForSTaps);
        xlatorForS.init(nullptr, hissCut - (2700.0 / 2 - hissSize), trxAudioSampleRate);
    }

    void draw(ImGui::WaterfallVFO* vfo);
};

struct QSOPanel {

    QSOPanel() : audioInProcessedIn(), audioInProcessed(&audioInProcessedIn) {
        audioIn.origin = "qsopanel.audioin";
        audioTowardsTransmitter.origin = "qsopanel.audioInToTransmitter";
        audioInProcessedIn.origin = "qsopanel.audioInProcessedIn";
        audioInProcessed.origin = "QSOPanel.audioInProcessed";
    }
    virtual ~QSOPanel();
    void startAudioPipeline();
    void init();
    void stopAudioPipeline();
    void draw(float currentFreq, ImGui::WaterfallVFO* pVfo);
    dsp::stream<dsp::stereo_t> audioIn;
    dsp::stream<dsp::stereo_t> audioInProcessedIn;
    dsp::routing::Splitter<dsp::stereo_t> audioInProcessed;
    dsp::stream<dsp::stereo_t> audioTowardsTransmitter;
    std::shared_ptr<ConfigPanel> configPanel;


    //    int readSamples;
    //    const int fftFPS = 60;
    std::shared_ptr<AudioFFTForDisplay> audioInToFFT;
    std::shared_ptr<AudioInToTransmitter> audioInToTransmitter;
    std::shared_ptr<std::thread> receiveBuffers;
    dsp::arrays::FloatArray currentFFTBuffer;
    std::mutex currentFFTBufferMutex;
    void handleTxButton(ImGui::WaterfallVFO* vfo, bool tx, bool tune);
    float currentFreq;
    int txGain = 0;

    float micGain = 0; // in db
    bool enablePA = false;
    bool transmitting = false;
    void setModeSubmode(const std::string& mode, const std::string& submode);
    std::string submode;
    std::string mode;
    EventHandler<float> maxSignalHandler;
    std::atomic<float> maxSignal = 0.0;
    bool postprocess = true;
    int triggerTXOffEvent = 0;
    std::vector<float> lastSWR, lastForward, lastReflected;
    void drawHistogram();

    float maxTxSignalPeak = 0.0;
    int64_t maxTxSignalPeakTime = 0;
};


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
    auto avail = ImGui::GetContentRegionAvail();
    auto window = ImGui::GetCurrentWindow();

    auto mouseCoord = ImVec2(ImGui::GetMousePos().x - widgetPos.x, ImGui::GetMousePos().y - widgetPos.y);

    //    auto diameter = std::min<float>(avail.x, avail.y);
    //    float radius = diameter / 2 * this->sizeFactor;


    ImU32 bg0 = ImGui::ColorConvertFloat4ToU32(ImVec4(0.5f, 0.5f, 0.5f, 1.0f));
    //    const ImVec2& buttonCenter = avail / 2;

    bool pressed = ImGui::IsAnyMouseDown() && ImGui::GetTopMostPopupModal() == NULL;
    bool startedInside = withinRect(avail, this->pressPoint);
    ImGuiContext& g = *GImGui;
    if (pressed) {
        const float t = g.IO.MouseDownDuration[0];
        if (t < 0.2) {
            if (isnan(this->pressPoint.x)) {
                this->pressPoint = mouseCoord;
                if (withinRect(avail, this->pressPoint)) {
                    this->currentlyPressed = true; // not cleared whe   n finger away of button
                    this->currentlyPressedTime = currentTimeMillis();
                }
            }
        }
    }
    else {
        this->currentlyPressed = false;
        if (startedInside && withinRect(avail, mouseCoord)) {
            // released inside
            if (currentlyPressedTime != 0) { // handle release, longpress not happened
                retval = true;
            }
        }
        currentlyPressedTime = 0;
        this->pressPoint = ImVec2((float)nan(""), (float)nan(""));
    }

    if (pressed && startedInside && withinRect(avail, mouseCoord)) {
        if (this->currentlyPressedTime != 0) {
            bg0 = ImGui::ColorConvertFloat4ToU32(ImVec4(1.0f, 1.0f, 0.5f, 1.0f));
        }
        else {
            // remove the highlight after long press
        }
    }

    window->DrawList->AddRectFilled(windowPos, windowPos + avail, bg0, (float)avail.y / 10.0);


    //    auto ts = ImGui::CalcTextSize(this->upperText.c_str());
    //    ImGui::SetCursorPos((ImVec2{avail.x, 0} - ImVec2{ts.x, 0}) / 2);
    //    ImGui::Text("%s", this->upperText.c_str());

    const char* buttonTextStr = this->buttonText.c_str();
    //    if (this->buttonText.size() == 1) {
    //        ImGui::PushFont(style::bigFont);
    //    }
    auto ts = ImGui::CalcTextSize(buttonTextStr);
    ImGui::SetCursorPos((avail - ImVec2{ ts.x, ts.y }) / 2);
    ImGui::Text("%s", buttonTextStr);
    //    if (this->buttonText.size() == 1) {
    //        ImGui::PopFont();
    //    }
    ImGui::PopFont();

    return retval;
}

double TheEncoder::draw(float currentValue) {
    static int idSeq = 0;
    static int currentEncoderTouch = -1;

    if (encoderId < 0) {
        encoderId = idSeq++;
    }
    auto avail = ImGui::GetContentRegionAvail();
    int MARKS_PER_ENCODER = 20;
    float retval = 0;


    float R = avail.y / 2;
    auto window = ImGui::GetCurrentWindow();
    auto widgetPos = ImGui::GetWindowPos();
    auto DRAW_HEIGHT_PERCENT = 0.95f;

    ImU32 bg0 = ImGui::ColorConvertFloat4ToU32(ImVec4(0.0f, 0.0f, 0.0f, 1.0f));
    window->DrawList->AddRectFilled(widgetPos, widgetPos + avail, bg0);

    for (int q = 0; q < MARKS_PER_ENCODER; q++) {
        auto markY = (R * DRAW_HEIGHT_PERCENT) * (float)sin(M_PI * 2 / MARKS_PER_ENCODER * q + this->somePosition);
        auto markColor = (float)cos(M_PI * 2 / MARKS_PER_ENCODER * q + this->somePosition);
        if (markColor > 0) {
            ImU32 col = ImGui::ColorConvertFloat4ToU32(ImVec4(1, 1, 1, markColor));
            window->DrawList->AddRectFilled(widgetPos + ImVec2(0, R + markY - 4), widgetPos + ImVec2(avail.x, R + markY + 4), col);
        }
    }

    if (enabled) {
        if (ImGui::IsAnyMouseDown()) {
            if (currentEncoderTouch != -1 && currentEncoderTouch != encoderId) {
                return 0;
            }
//            flog::info("currentEncoderTouch={} encoderId={}", currentEncoderTouch, encoderId);
            auto mouseX = ImGui::GetMousePos().x - widgetPos.x;
            auto mouseY = ImGui::GetMousePos().y - widgetPos.y;
            bool mouseInside = mouseX >= 0 && mouseX < avail.x && mouseY >= 0 && mouseY < avail.y;
            if (!mouseInside && isnan(lastMouseAngle)) {
                // clicked outside
            }
            else {
                // can continue everywhere
                auto mouseAngle = (ImGui::GetMousePos().y - widgetPos.y - R) / (R * DRAW_HEIGHT_PERCENT);
                if (!isnan(lastMouseAngle)) {
                    this->somePosition += mouseAngle - lastMouseAngle;
                    retval = (float)(mouseAngle - lastMouseAngle);
                }
                else {
                    // first click!
                    this->currentValue = (float)currentValue;
                    currentEncoderTouch = encoderId;
                }
                lastMouseAngle = mouseAngle;
                this->speed = 0;
                fingerMovement.emplace_back(lastMouseAngle);
            }
        }
        else {
            // finger up
            if (currentEncoderTouch == encoderId) {
                currentEncoderTouch = -1;
                auto sz = this->fingerMovement.size();
                if (sz >= 2) {
                    this->speed = (this->fingerMovement[sz - 1] - this->fingerMovement[sz - 2]) / 2;
                }
                this->fingerMovement.clear();
                lastMouseAngle = nan("");
            }
        }

        // inertia
        if (fabs(speed) < 0.001) {
            if (speed != 0) {
                speed = 0;
            }
        }
        this->somePosition += speed;
        if (speed != 0) {
            retval = (float)speed;
        }
        this->speed *= this->delayFactor;
        return retval;
    }
    else {
        this->speed = 0;
        return 0;
    }
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
        if (submode == "CW") selectedDemod = RadioModule::RADIO_DEMOD_CW;
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
    auto maybeFreq = getCurrentModeAttr("freq_" + submode + "_" + band);
    this->bandUp.upperText = band;
    if (!maybeFreq.empty()) {
        double currentFreq = strtod(maybeFreq.c_str(), nullptr);
        tuner::tune(tuner::TUNER_MODE_CENTER, gui::waterfall.selectedVFO, currentFreq);
        return;
    }
    auto nfreq = this->frequencyDefaults[band + "_" + submode];
    if (nfreq == 0)
        nfreq = this->frequencyDefaults[band + "_" + mode];
    if (nfreq == 0)
        nfreq = this->frequencyDefaults[band];
    if (nfreq != 0) {
        tuner::tune(tuner::TUNER_MODE_CENTER, gui::waterfall.selectedVFO, nfreq * 1000.0);
    }
}

void MobileMainWindow::autoDetectBand(int frequency) {
    static int lastFrequency = 0;
    static const std::string* lastFreqBand = nullptr;
    const std::string* newBand;

    if (frequency == lastFrequency) {
        newBand = lastFreqBand;
    }
    else {
        newBand = &this->getBand(frequency);
        lastFrequency = frequency;
        lastFreqBand = newBand;
    }
    if (*newBand != this->bandUp.upperText) {
        this->bandUp.upperText = *newBand;
    }
}

void MobileMainWindow::updateAudioWaterfallPipeline() {
    if (gui::waterfall.selectedVFO != currentAudioStreamName) {
        if (!currentAudioStreamName.empty()) {
            currentAudioStream->stopReader();
            sigpath::sinkManager.unbindStream(currentAudioStreamName, currentAudioStream);
        }
        currentAudioStreamName = gui::waterfall.selectedVFO;
        if (currentAudioStreamName.empty()) {
            currentAudioStream = nullptr;
        }
        else {
            currentAudioStreamSampleRate = (int)sigpath::sinkManager.getStreamSampleRate(currentAudioStreamName);
            currentAudioStream = sigpath::sinkManager.bindStream(currentAudioStreamName);
            std::thread x([&]() {
                while (true) {
                    int rd = currentAudioStream->read();
                    if (rd < 0) {
                        break;
                    }
                    audioWaterfall->addAudioSamples(currentAudioStream->readBuf, rd, currentAudioStreamSampleRate);
                    currentAudioStream->flush();
                }
            });
            x.detach();
        }
    }
    if (currentAudioStreamName != "") {
        currentAudioStreamSampleRate = (int)sigpath::sinkManager.getStreamSampleRate(currentAudioStreamName);
    }
}

void MobileMainWindow::draw() {

    updateAudioWaterfallPipeline();


    gui::waterfall.alwaysDrawLine = false;
    if (displaymenu::transcieverLayout == 0) {
        shouldInitialize = true;
        MainWindow::draw();
        return;
    }
    if (shouldInitialize) {
        shouldInitialize = false;
        if (getCurrentBand().empty()) {
            selectCurrentBand("20M", -1);
        }
        bandUp.upperText = this->getCurrentBand();
        auto currentMode = this->getCurrentMode();
        modeToggle.upperText = currentMode;
        updateSubmodeAfterChange();
        zoomToggle.upperText = "custom";
    }
    gui::waterfall.alwaysDrawLine = true;
    ImGui::WaterfallVFO* vfo = nullptr; // gets initialized below
    this->preDraw(&vfo);

    ImGui::Begin("Main", nullptr, WINDOW_FLAGS);

    ImVec4 textCol = ImGui::GetStyleColorVec4(ImGuiCol_Text);
    ImVec2 btnSize(30 * style::uiScale, 30 * style::uiScale);
    ImGui::PushID((int)ImGui::GetID("sdrpp_menu_btn"));
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
    switch (qsoMode) {
    case VIEW_QSO:
        buttonsWidth = 600;
        break;
    case VIEW_CONFIG:
        buttonsWidth = style::baseFont->FontSize * 25;
        break;
    default:
        break;
    }

    const ImVec2 waterfallRegion = ImVec2(ImGui::GetContentRegionAvail().x - encoderWidth - buttonsWidth, ImGui::GetContentRegionAvail().y - statusHeight);

    lockWaterfallControls = showMenu || (qsoMode != VIEW_DEFAULT && modeToggle.upperText == "CW");
    ImGui::BeginChildEx("Waterfall", ImGui::GetID("sdrpp_waterfall"), waterfallRegion, false, 0);
    auto waterfallStart = ImGui::GetCursorPos();
    gui::waterfall.draw();
    ImGui::SetCursorPos(waterfallStart + ImVec2(gui::waterfall.fftAreaMin.x + 5 * style::uiScale, 0));
    ImGui::PushFont(style::mediumFont);
    ImGui::Text("BAND: %s", this->bandUp.upperText.c_str());
    ImGui::PopFont();

    onWaterfallDrawn.emit(GImGui);
    ImGui::EndChild();

    this->handleWaterfallInput(vfo);

    ImGui::SetCursorPos(cornerPos + ImVec2(gui::waterfall.fftAreaMin.x + 5 * style::uiScale, waterfallRegion.y));
    ImGui::PushFont(style::mediumFont);
    ImGui::Text("%s -> %s    zoom: %s",
                this->modeToggle.upperText.c_str(),
                this->submodeToggle.upperText.c_str(),
                this->zoomToggle.upperText.c_str());
    ImGui::PopFont();

    ImGui::SetCursorPos(cornerPos);


    if (showMenu) {
        menuWidth = core::configManager.conf["menuWidth"];
        const ImVec2 menuRegion = ImVec2((float)menuWidth, ImGui::GetContentRegionAvail().y);
        ImGui::BeginChildEx("Menu", ImGui::GetID("sdrpp_menu"), menuRegion, false, 0);
        ImU32 bg = ImGui::ColorConvertFloat4ToU32(gui::themeManager.waterfallBg);
        auto window = ImGui::GetCurrentWindow();
        auto widgetPos = ImGui::GetWindowContentRegionMin();
        auto rectRegion = menuRegion;
        rectRegion.y += 10000; // menu is scrolling, rect somehow scrolls with it, so must be big enough to be always visible
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

    ImGui::SetCursorPos(cornerPos + ImVec2{ waterfallRegion.x + buttonsWidth, 0 });
    float encodersHeight = ImGui::GetContentRegionAvail().y;
    ImGui::BeginChildEx("Small Encoder", ImGui::GetID("sdrpp_small_encoder"), ImVec2(encoderWidth, encodersHeight / 3), false, 0);

    double smallChangeSpeed = smallEncoder.draw(2000); // random value
    ImGui::EndChild();
    switch (smallWheelFunctionN) {
    case 0: { // zoom
        auto nbw = bw + smallChangeSpeed / 5;
        if (nbw < 0) {
            nbw = 0;
        }
        if (nbw > 1) {
            nbw = 1;
        }
        if (bw != nbw) {
            bw = nbw;
            core::configManager.acquire();
            core::configManager.conf["zoomBw"] = bw;
            core::configManager.release(true);
            updateWaterfallZoomBandwidth(bw);
        }
        break;
    }
    case 1: { // brightness
        auto nfftMin = fftMin + smallChangeSpeed * 10;
        if (nfftMin < -200) {
            nfftMin = -200;
        }
        if (nfftMin > 0) {
            nfftMin = 0;
        }
        nfftMin = std::min<float>(fftMax - 10, nfftMin);
        if (nfftMin != fftMin) {
            fftMin = nfftMin;
            core::configManager.acquire();
            core::configManager.conf["min"] = fftMin;
            core::configManager.release(true);
            gui::waterfall.setWaterfallMin(fftMin);
        }
        break;
    }
    case 2: { // volume

        auto vfoName = gui::waterfall.selectedVFO;
        if (vfoName == "" || sigpath::sinkManager.streams.find(vfoName) == sigpath::sinkManager.streams.end()) {
            break;
        }
        auto stream = sigpath::sinkManager.streams[vfoName];
        auto nvol = stream->getVolume() - smallChangeSpeed / 5;
        if (nvol < 0) {
            nvol = 0;
        }
        if (nvol > 1) {
            nvol = 1;
        }
        if (nvol != stream->getVolume()) {
            stream->setVolume(nvol);
        }
        break;
    }
    }
//    flog::info("small encoder speed: {}", smallChangeSpeed);
    ImGui::SetCursorPos(cornerPos + ImVec2{ waterfallRegion.x + buttonsWidth, +encodersHeight / 3 });
    ImGui::BeginChildEx("Encoder", ImGui::GetID("sdrpp_encoder"), ImVec2(encoderWidth, encodersHeight * 2 / 3), false, 0);
    auto currentFreq = vfo ? (vfo->generalOffset + gui::waterfall.getCenterFrequency()) : gui::waterfall.getCenterFrequency();
    double offsetDelta = encoder.draw(currentFreq);
    if (offsetDelta != 0) {
        auto externalFreq = vfo ? (vfo->generalOffset + gui::waterfall.getCenterFrequency()) : gui::waterfall.getCenterFrequency();
        if (fabs(externalFreq - encoder.currentValue) > 10000) {
            // something changed! e.g. changed band when encoder was rotating
            encoder.speed = 0;
        }
        else {
            double od = offsetDelta * 100;
            double sign = od < 0 ? -1 : 1;
            if (fabs(od) < 2) {
                od = 0.5 * sign;
            }
            else {
                od = fabs(od) - 1; // 4 will become 1
                od = pow(od, 1.4) * sign;
                //                od *= 5;
            }
            flog::info("od={} encoder.currentFrequency={}", od, encoder.currentValue);
            encoder.currentValue -= od;
            auto cf = ((int)(encoder.currentValue / 10)) * 10.0;
            tuner::tune(tuningMode, gui::waterfall.selectedVFO, cf);
        }
    }
    //    auto currentFreq = vfo ? (vfo->generalOffset + gui::waterfall.getCenterFrequency()) : gui::waterfall.getCenterFrequency();
    this->autoDetectBand((int)currentFreq);
    ImGui::EndChild(); // encoder


    ImGui::SetCursorPos(cornerPos + ImVec2{ waterfallRegion.x, 0 });
    auto vertPadding = ImGui::GetStyle().WindowPadding.y;


    MobileButton* buttonsDefault[] = { &this->qsoButton, &this->zoomToggle, &this->modeToggle, &this->autoWaterfall, &this->audioConfigToggle, &this->smallWheelFunction /*&this->bandUp, &this->bandDown, &this->submodeToggle,*/ };
    MobileButton* buttonsQso[] = { &this->endQsoButton, &this->txButton, &this->softTune, &this->audioConfigToggle, &this->smallWheelFunction, &this->lockFrequency };
    MobileButton* buttonsConfig[] = { &this->exitConfig };
    auto nButtonsQso = (int)((sizeof(buttonsQso) / sizeof(buttonsQso[0])));
    auto nButtonsDefault = (int)((sizeof(buttonsDefault) / sizeof(buttonsDefault[0])));
    auto nButtonsConfig = (int)((sizeof(buttonsConfig) / sizeof(buttonsConfig[0])));
    auto buttonsSpaceY = ImGui::GetContentRegionAvail().y;
    switch (this->qsoMode) {
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
        configPanel->draw(vfo);
        ImGui::EndChild(); // buttons
    }
    if (this->qsoMode == VIEW_QSO) {
        ImGui::BeginChildEx("QSO", ImGui::GetID("sdrpp_qso"), ImVec2(buttonsWidth, ImGui::GetContentRegionAvail().y), false, 0);
        qsoPanel->draw((float)currentFreq, vfo);
        //        auto afterQSOPanel = ImGui::GetCursorPos();
        ImGui::EndChild(); // buttons

        //        ImGui::SetCursorPos(ImVec2{beforeQSOPanel.x, beforeQSOPanel.y + afterQSOPanel.y});
        //        buttonsSpaceY -= afterQSOPanel.y;
    }
    ImGui::SetCursorPos(beforePanel);


    MobileButton** buttons;
    int NBUTTONS;
    switch (this->qsoMode) {
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
    MobileButton* pressedButton = nullptr;
    auto buttonsStart = ImGui::GetCursorPos();
    auto intButtonHeight = style::baseFont->FontSize * 3.3;
    for (auto b = 0; b < NBUTTONS; b++) {
        char chi[100];
        sprintf(chi, "mob_button_%d", b);
        const ImVec2 childRegion = ImVec2(defaultButtonsWidth, intButtonHeight - vertPadding);
        if (this->qsoMode == VIEW_DEFAULT && false) {
            // align from top
            ImGui::SetCursorPos(buttonsStart + ImVec2{ 0, buttonsSpaceY - float(nButtonsDefault - b) * (childRegion.y + vertPadding) });
        }
        else {
            // align from bottom
            ImGui::SetCursorPos(buttonsStart + ImVec2{ buttonsWidth - defaultButtonsWidth, buttonsSpaceY - float(b + 1) * (childRegion.y + vertPadding) });
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
        ImVec2 childRegion(waterfallRegion.x / 2, waterfallRegion.y / 2);
        ImGui::BeginChildEx("cwbuttons", ImGui::GetID("cwbuttons"), childRegion, true, 0);
        cwPanel->draw();
        ImGui::EndChild(); // buttons
    }

    ImGuiIO& io = ImGui::GetIO();
    if (drawAudioWaterfall) {
        ImVec2 sz(io.DisplaySize.x / 6, io.DisplaySize.y / 4);
        audioWaterfall->draw(ImVec2(io.DisplaySize.x / 2 - sz.x / 2, io.DisplaySize.y - sz.y - 20), sz);
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
        if (vfo) {
            gui::waterfall.setViewOffset(vfo->centerOffset); // center vfo on screen
        }
    };
    if (this->zoomToggle.isLongPress()) {
        makeZoom(0);
    }
    if (pressedButton == &this->autoWaterfall) {
        gui::waterfall.autoRange();
    }
    //
    if (pressedButton == &this->lockFrequency) {
        encoder.enabled = !encoder.enabled;
    }
    if (!encoder.enabled && this->lockFrequency.buttonText != "Unlock") {
        this->lockFrequency.buttonText = "Unlock";
    }
    if (encoder.enabled && this->lockFrequency.buttonText != "Lock") {
        this->lockFrequency.buttonText = "Lock";
    }
    //
    if (pressedButton == &this->smallWheelFunction) {
        this->smallWheelFunctionN = (this->smallWheelFunctionN + 1) % nSmallWheelFunctions;
        this->smallWheelFunction.buttonText = smallWheelFunctionName(this->smallWheelFunctionN);
    }
    if (pressedButton == &this->audioConfigToggle) {
        if (!this->qsoPanel->audioInToFFT) {
            this->qsoPanel->startAudioPipeline();
        }
        this->prevMode = this->qsoMode;
        this->qsoMode = VIEW_CONFIG;
    }
    if (pressedButton == &this->exitConfig) {
        this->qsoMode = this->prevMode;
        if (this->qsoMode == VIEW_DEFAULT) {
            this->qsoPanel->stopAudioPipeline();
        }
    }
    if (pressedButton == &this->zoomToggle) {
        int selectedIndex = -1;
        for (auto q = 0; q < zooms.size(); q++) {
            if (zooms[q].first == zoomToggle.upperText) {
                selectedIndex = q;
                break;
            }
        }
        selectedIndex = int((selectedIndex + 1) % zooms.size());
        makeZoom(selectedIndex);
    }

    if (pressedButton == &this->bandUp || pressedButton == &this->bandDown) {
        auto cb = getCurrentBand();
        auto cbIter = std::find(bands.begin(), bands.end(), cb);
        if (cbIter != bands.end()) {
            auto cbIndex = cbIter - bands.begin();
            if (pressedButton == &this->bandUp) {
                cbIndex = (int)(cbIndex + 1) % bands.size();
            }
            else {
                cbIndex = (int)(cbIndex + bands.size() - 1) % bands.size();
            }
            auto newBand = bands[cbIndex];
            selectCurrentBand(newBand, (int)currentFreq);
            if (getCurrentMode() == "SSB") {
                this->selectSSBModeForBand(newBand);
            }
            updateFrequencyAfterChange();
        }
    }
    const char* TxModePopup = "TX/RX Mode Select";
    if (pressedButton == &this->modeToggle) {
        ImGui::OpenPopup(TxModePopup);

        //        auto curr = std::find(this->modes.begin(), this->modes.end(), getCurrentMode());
        //        if (curr != this->modes.end()) {
        //            auto currIndex = curr - this->modes.begin();
        //            currIndex = int(currIndex + 1) % this->modes.size();
        //            auto newMode = this->modes[currIndex];
        //            this->leaveBandOrMode((int)currentFreq);
        //            setCurrentMode(newMode);
        //            pressedButton->upperText = newMode;
        //            updateSubmodeAfterChange();
        //        }
    }
    if (pressedButton == &this->qsoButton) {
        this->qsoMode = VIEW_QSO;
        qsoPanel->startAudioPipeline();
    }
    if (pressedButton == &this->endQsoButton) {
        this->qsoMode = VIEW_DEFAULT;
        qsoPanel->stopAudioPipeline();
    }
    if (pressedButton == &this->submodeToggle) {
        std::vector<std::string>& submos = subModes[getCurrentMode()];
        auto submoIter = std::find(submos.begin(), submos.end(), getCurrentModeAttr("submode"));
        if (submoIter != submos.end()) {
            auto submoIndex = submoIter - submos.begin();
            submoIndex = int(submoIndex + 1) % submos.size();
            auto newSubmode = submos[submoIndex];
            setCurrentModeAttr("submode", newSubmode);
            updateSubmodeAfterChange();
        }
    }
    qsoPanel->setModeSubmode(modeToggle.upperText, submodeToggle.upperText);
    if (sigpath::transmitter && qsoPanel->triggerTXOffEvent < 0) { // transiver exists, and TX is not in handing off state
        if (pressedButton == &this->softTune) {
            if (qsoPanel->audioInToTransmitter) {
                // tuning, need to stop
                qsoPanel->handleTxButton(vfo, false, true);
#define SOFT_TUNE_LABEL "SoftTune"
                pressedButton->buttonText = SOFT_TUNE_LABEL;
            }
            else {
                // need to start
                qsoPanel->handleTxButton(vfo, true, true);
                pressedButton->buttonText = "Tuning..";
            }
        }
        bool txPressed = txButton.currentlyPressed || ((ImGui::IsKeyDown(ImGuiKey_VolumeUp) || ImGui::IsKeyDown(ImGuiKey_Insert)) && qsoMode == VIEW_QSO);
        if (txPressed && !qsoPanel->audioInToTransmitter || !txPressed && qsoPanel->audioInToTransmitter && qsoPanel->audioInToTransmitter->tuneFrequency == 0) {
            // button is pressed ok to send, or button is released and audioInToTransmitter running and it is not in tune mode
            qsoPanel->handleTxButton(vfo, txPressed, false);
            //
        }
    }
    if (ImGui::BeginPopupModal(TxModePopup, nullptr, ImGuiWindowFlags_AlwaysAutoResize)) {
        ImVec2 screenSize = io.DisplaySize;
        screenSize.x *= 0.8;
        screenSize.y *= 0.8;
        ImGui::SetWindowSize(screenSize);
        ImGui::BeginChild("##TxModePopup", ImVec2(screenSize.x, screenSize.y - style::baseFont->FontSize * 4), true, 0);
        auto currentBand = getCurrentBand();
        ImGui::Text("Band (wavelength meters):");
        ImGui::NewLine();
        for (auto& b : bands) {
            ImGui::SameLine();
            bool pressed = false;
            if (currentBand == b) {
                pressed = doFingerButton(">" + b + "<");
            }
            else {
                pressed = doFingerButton(b);
            }
            if (pressed && b != currentBand) {
                selectCurrentBand(b, (int)currentFreq);
                if (getCurrentMode() == "SSB") {
                    this->selectSSBModeForBand(b);
                }
                updateFrequencyAfterChange();
            }
        }
        ImGui::NewLine();
        ImGui::Text("Mode / Submode:");
        ImGui::NewLine();
        auto currentSubmode = getCurrentModeAttr("submode");
        std::string currentMode = "";
        for (auto& b : modes) {
            ImGui::SameLine();
            for (auto& bb : subModes[b]) {
                bool pressed = false;
                ImGui::SameLine();
                if (bb == currentSubmode) {
                    pressed = doFingerButton(">" + bb + "<");
                }
                else {
                    pressed = doFingerButton(bb);
                }
                if (pressed && bb != currentSubmode) {
                    setCurrentMode(b);
                    setCurrentModeAttr("submode", bb);
                    updateSubmodeAfterChange();
                    currentSubmode = bb;
                }
                if (bb == currentSubmode) {
                    currentMode = b;
                }
            }
        }
        ImGui::NewLine();
        ImGui::Text("Bandwidth:");
        ImGui::NewLine();
        auto* bw = &ssbBandwidths;
        if (currentMode == "CW") {
            bw = &cwBandwidths;
        }
        if (currentMode == "AM") {
            bw = &amBandwidths;
        }
        if (currentMode == "FM") {
            bw = &fmBandwidths;
        }
        auto bandwidthk = vfo->bandwidth / 1000.0;
        char fmt[32];
        sprintf(fmt, "%0.1f", bandwidthk);
        //        if (bandwidthk == floor(bandwidthk)) {
        //            sprintf(fmt, "%0.0f", bandwidthk);
        //        } else {
        //        }
        for (auto& b : *bw) {
            ImGui::SameLine();
            bool pressed = false;
            if (b == fmt) {
                pressed = doFingerButton(">" + b + "<");
            }
            else {
                pressed = doFingerButton(b);
            }
            if (pressed) {
                if (bw == &cwBandwidths) {
                    vfo->setBandwidth(std::stof(b)); // in hz
                }
                else {
                    vfo->setBandwidth(std::stof(b) * 1000); // in khz
                }
            }
        }
        ImGui::EndChild();
        if (doFingerButton("Close")) {
            ImGui::CloseCurrentPopup();
        }
        ImGui::EndPopup();
    }

    else if (!sigpath::transmitter) {
        if (txButton.currentlyPressed) {
            sigpath::sinkManager.toneGenerator.store(1);
        }
        else {
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
    setCurrentModeAttr("freq_" + submode + "_" + leavingBand, std::to_string(leavingFrequency));
    if (submode == "USB" || submode == "LSB") {
        setCurrentModeAttr("band_" + leavingBand + "_ssb", submode);
    }
}

void MobileMainWindow::selectSSBModeForBand(const std::string& band) {
    auto maybeSubmode = getCurrentModeAttr("band_" + band + "_ssb");
    if (!maybeSubmode.empty()) {
        if (bandsLimits[band].first >= 10000000) {
            maybeSubmode = "USB";
        }
        else {
            maybeSubmode = "LSB";
        }
    }
    setCurrentModeAttr("submode", maybeSubmode);
    updateSubmodeAfterChange();
}

void MobileMainWindow::selectCurrentBand(const std::string& band, int leavingFrequency) {
    if (getCurrentBand() != "" && leavingFrequency > 0) {
        this->leaveBandOrMode(leavingFrequency);
    }
    core::configManager.acquire();
    core::configManager.conf["mobileBand"] = band;
    core::configManager.release(true);
}

std::string MobileMainWindow::getCurrentModeAttr(const std::string& key) {
    std::string retval;
    auto currentMode = getCurrentMode();
    core::configManager.acquire();
    json x = core::configManager.conf["mobileRadioMode_" + currentMode];
    if (!x.empty()) {
        if (x.find(key) != x.end()) {
            retval = x[key];
        }
    }
    core::configManager.release(false);
    return retval;
}

void MobileMainWindow::setCurrentModeAttr(const std::string& key, std::string val) {
    auto currentMode = getCurrentMode();
    core::configManager.acquire();
    json x = core::configManager.conf["mobileRadioMode_" + currentMode];
    if (!x.empty()) {
        x[key] = val;
        core::configManager.conf["mobileRadioMode_" + currentMode] = x;
    }
    else {
        x = json();
        x[key] = val;
        core::configManager.conf["mobileRadioMode_" + currentMode] = x;
    }
    core::configManager.release(true);
}

const std::string& MobileMainWindow::getBand(int frequency) {
    static std::string empty;
    for (auto& b : bandsLimits) {
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
                                       smallWheelFunction("", smallWheelFunctionName(0)),
                                       audioConfigToggle("", "Audio Cfg"),
                                       autoWaterfall("", "Waterfall!"),
                                       modeToggle("SSB", "Mode"),
                                       submodeToggle("LSB", "Submode"),
                                       qsoButton("", "QSO"),
                                       endQsoButton("", "End QSO"),
                                       txButton("", "TX"),
                                       exitConfig("", "OK"),
                                       lockFrequency("", "Lock"),
                                       softTune("", SOFT_TUNE_LABEL) {
    qsoPanel = std::make_shared<QSOPanel>();
    configPanel = std::make_shared<ConfigPanel>(&qsoPanel->audioInProcessed);
    qsoPanel->configPanel = configPanel;
    cwPanel = std::make_shared<CWPanel>();
    audioWaterfall = std::make_shared<SubWaterfall>(trxAudioSampleRate, "Audio Heard");
}

void MobileMainWindow::init() {
    MainWindow::init();
    configPanel->init();
    audioWaterfall->init();
    qsoPanel->init();
    omlsa_setResDir(core::configManager.conf["resourcesDirectory"]);
    core::configManager.acquire();
    if (core::configManager.conf.contains("showAudioWaterfall")) {
        drawAudioWaterfall = core::configManager.conf["showAudioWaterfall"];
    }
    core::configManager.release(true);

    displaymenu::onDisplayDraw.bindHandler(&displayDrawHandler);
    displayDrawHandler.handler = [](ImGuiContext* ctx, void* data) {
        MobileMainWindow* _this = (MobileMainWindow*)data;
        if (ImGui::Checkbox("Show Audio Waterfall##_sdrpp", &_this->drawAudioWaterfall)) {
            core::configManager.acquire();
            core::configManager.conf["showAudioWaterfall"] = _this->drawAudioWaterfall;
            core::configManager.release(true);
        }
    };
    displayDrawHandler.ctx = this;
}

void MobileMainWindow::end() {
    displaymenu::onDisplayDraw.unbindHandler(&displayDrawHandler);
    MainWindow::end();
    qsoPanel.reset();
    cwPanel.reset();
    configPanel.reset();
}


void QSOPanel::startAudioPipeline() {
    if (!configPanel->afnr) {
        configPanel->afnr = std::make_shared<dsp::AFNR_OMLSA_MCRA>();
        configPanel->afnr->allowed = true;
        getConfig("trx_afnrAllowd", configPanel->afnr->allowed);
        getConfig("trx_SqlAllowd", configPanel->micSql);
        getConfig("trx_SqlLevel", configPanel->micSqlLevel);
        configPanel->afnr->init(nullptr);
        configPanel->afnr->omlsa_mcra.setSampleRate(trxAudioSampleRate);
    }
    audioIn.clearReadStop();
    sigpath::sinkManager.defaultInputAudio.bindStream(&audioIn);
    std::thread inputProcessor([this] {
        int64_t start = currentTimeMillis();
        int64_t count = 0;

        dsp::filter::FIR<dsp::complex_t, dsp::complex_t> hipass;
        dsp::filter::FIR<dsp::stereo_t, float> lopass;
        auto prevHighPass = configPanel->highPass;
        auto prevLowPass = configPanel->lowPass;
        auto hipassTaps = dsp::taps::highPass0<dsp::complex_t>(prevHighPass, 30, trxAudioSampleRate);
        hipass.init(nullptr, hipassTaps);
        auto lopassTaps = dsp::taps::lowPass0<float>(prevLowPass, 300, trxAudioSampleRate);
        lopass.init(nullptr, lopassTaps);
        dsp::noise_reduction::Squelch squelch;
        squelch.init(nullptr, -40);

        while (true) {
            int rd = audioIn.read();
            //            flog::info("audioIn.read() = {}", rd);
            if (rd < 0) {
                flog::info("Breaking the tx audio in loop");
                break;
            }
            configPanel->rawInDecibels.addSamples(audioIn.readBuf, rd);
            if (configPanel->lowPass != prevLowPass) {
                dsp::taps::free(lopassTaps);
                lopassTaps = dsp::taps::lowPass0<float>(configPanel->lowPass, 200, trxAudioSampleRate);
                lopass.setTaps(lopassTaps);
                prevLowPass = configPanel->lowPass;
            }
            if (configPanel->highPass != prevHighPass) {
                dsp::taps::free(hipassTaps);
                hipassTaps = dsp::taps::highPass0<dsp::complex_t>(configPanel->highPass, 300, trxAudioSampleRate);
                hipass.setTaps(hipassTaps);
                prevHighPass = configPanel->highPass;
            }
            configPanel->s2m.process(rd, audioIn.readBuf, configPanel->s2m.out.writeBuf);

            audioIn.flush();

            configPanel->r2c.process(rd, configPanel->s2m.out.writeBuf, configPanel->r2c.out.writeBuf);

            if (configPanel->hissAdd) {
                configPanel->highPassForS.process(rd, configPanel->r2c.out.writeBuf, configPanel->highPassForS.out.writeBuf);
                configPanel->xlatorForS.process(rd, configPanel->highPassForS.out.writeBuf, configPanel->xlatorForS.out.writeBuf);
                for (int q = 0; q < rd; q++) {
                    //                    r2c.out.writeBuf[q] = xlatorForS.out.writeBuf[q];
                    configPanel->r2c.out.writeBuf[q] = configPanel->r2c.out.writeBuf[q] * 0.5 + (configPanel->xlatorForS.out.writeBuf[q] * 2.5);
                }
            }


            hipass.process(rd, configPanel->r2c.out.writeBuf, hipass.out.writeBuf);
            configPanel->highPassDecibels.addSamples(hipass.out.writeBuf, rd);

            if (configPanel->doEqualize) {
                configPanel->equalizer.process(rd, hipass.out.writeBuf, configPanel->equalizer.out.writeBuf);
                auto mult = pow(10, configPanel->compAmp / 20);
                for (int i = 0; i < rd; i++) {
                    configPanel->equalizer.out.writeBuf[i] *= mult;
                }
            }
            else {
                memcpy(configPanel->equalizer.out.writeBuf, hipass.out.writeBuf, rd * sizeof(dsp::complex_t));
            }
            configPanel->equalizerDecibels.addSamples(configPanel->equalizer.out.writeBuf, rd);
            if (configPanel->doFreqMaim) {
                configPanel->freqMaim.process(rd, configPanel->equalizer.out.writeBuf, configPanel->freqMaim.out.writeBuf);
            }
            else {
                memcpy(configPanel->freqMaim.out.writeBuf, configPanel->equalizer.out.writeBuf, rd * sizeof(dsp::complex_t));
            }
            configPanel->c2r.process(rd, configPanel->freqMaim.out.writeBuf, configPanel->c2r.out.writeBuf);
            configPanel->agc.process(rd, configPanel->c2r.out.writeBuf, configPanel->agc.out.writeBuf);
            configPanel->agcDecibels.addSamples(configPanel->agc.out.writeBuf, rd);
            configPanel->m2s.process(rd, configPanel->agc.out.writeBuf, configPanel->m2s.out.writeBuf);
            configPanel->afnr->process(configPanel->m2s.out.writeBuf, rd, configPanel->afnr->out.writeBuf, rd);
            configPanel->nrDecibels.addSamples(configPanel->afnr->out.writeBuf, rd);
            if (configPanel->micSql){
                squelch.setLevel(configPanel->micSqlLevel);
                squelch.process(rd, configPanel->afnr->out.writeBuf, configPanel->afnr->out.writeBuf);
            }
            lopass.process(rd, configPanel->afnr->out.writeBuf, audioInProcessedIn.writeBuf);
            if (rd > 0) {
                configPanel->outDecibels.addSamples(audioInProcessedIn.writeBuf, rd);
            }
            if (rd != 0) {
                count += rd;
                int64_t since = currentTimeMillis() - start;
                if (since == 0) {
                    since = 1;
                }
                //                flog::info("audioInProcessedIn.swap({}), samples/sec {}, count {} since {}", rd, count * 1000/since, count, since);
                if (!audioInProcessedIn.swap(rd)) {
                    break;
                }
            }
        }
    });
    inputProcessor.detach();


    audioInToFFT = std::make_shared<AudioFFTForDisplay>(&audioInProcessed);
    audioInToFFT->start();
    sigpath::averageTxSignalLevel.bindHandler(&maxSignalHandler);
    maxSignalHandler.ctx = this;
    maxSignalHandler.handler = [](float _maxSignal, void* ctx) {
        auto _this = (QSOPanel*)ctx;
        _this->maxSignal.store(_maxSignal);
    };
    audioInProcessed.start();
    flog::info("audioInProcessed.start()");
    std::thread x([this] {
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


void QSOPanel::setModeSubmode(const std::string& _mode, const std::string& _submode) {
    if (submode != _submode) {
        submode = _submode;
    }
    if (mode != _mode) {
        mode = _mode;
    }
}

void QSOPanel::handleTxButton(ImGui::WaterfallVFO* vfo, bool tx, bool tune) {
    if (tx) {
        if (!this->transmitting) {
            sigpath::txState.emit(tx);
            this->transmitting = true;
            if (sigpath::transmitter) {
                audioInProcessed.bindStream(&audioTowardsTransmitter);
                audioInToTransmitter = std::make_shared<AudioInToTransmitter>(&audioTowardsTransmitter);
                if (vfo) {
                    audioInToTransmitter->bandwidth = std::min<double>(12000.0, vfo->bandwidth);
                }
                else {
                    audioInToTransmitter->bandwidth = 2700;
                }
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
    }
    else {
        if (this->transmitting) {
            this->transmitting = false;
            triggerTXOffEvent = 14; // 60fps,
            if (sigpath::transmitter) {
                sigpath::transmitter->setTransmitStatus(false);
                audioInProcessed.unbindStream(&audioTowardsTransmitter);
                audioInToTransmitter->stop();
                audioInToTransmitter->out.stopReader();
            }
        }
    }
    configPanel->afnr->allowed2 = !this->transmitting;
}

void QSOPanel::stopAudioPipeline() {
    if (audioInToFFT) {
        sigpath::sinkManager.defaultInputAudio.unbindStream(&audioIn);
        flog::info("QSOPanel::stop. Calling audioInToFFT->stop()");
        audioInToFFT->stop();
        audioInToFFT->out.stopReader();
        audioInProcessed.stop();
        flog::info("Calling audioInToFFT->reset()");
        audioInToFFT.reset();
        audioIn.stopReader();
        flog::info("Reset complete");
        sigpath::averageTxSignalLevel.unbindHandler(&maxSignalHandler);
    }
}

float rtmax(std::vector<float>& v) {
    if (v.empty()) {
        return 0;
    }
    while (v.size() > 20) {
        v.erase(v.begin());
    }
    auto m = v[0];
    for (int q = 1; q < v.size(); q++) {
        if (v[q] > m) {
            m = v[q];
        }
    }
    return m;
}

void draw_db_gauge(float gauge_width, float level, float peak, float min_level = -140.0f, float max_level = 20.0f, float red_zone = 0.0f, int step = 10) {
    //
    // NB this has been produced by chatgpt 4.
    //
    float gauge_height = style::baseFont->FontSize * 1.5f;


    ImVec2 pos = ImGui::GetCursorScreenPos();
    ImVec2 gauge_min = ImVec2(pos.x, pos.y);
    ImVec2 gauge_max = ImVec2(pos.x + gauge_width, pos.y + gauge_height);

    // Draw the background
    ImGui::GetWindowDrawList()->AddRectFilled(gauge_min, gauge_max, ImGui::GetColorU32(ImGuiCol_FrameBg));

    // Draw the ticks and text
    for (int i = static_cast<int>(min_level); i <= static_cast<int>(max_level); ++i) {
        if (i % step == 0) {
            float x = (i - min_level) / (max_level - min_level) * gauge_width + pos.x;
            ImVec2 tick_start = ImVec2(x, pos.y);
            ImVec2 tick_end = ImVec2(x, pos.y + gauge_height * 0.3f);
            ImGui::GetWindowDrawList()->AddLine(tick_start, tick_end, ImGui::GetColorU32(ImGuiCol_Text), 1.0f);

            char text_buffer[16];
            snprintf(text_buffer, sizeof(text_buffer), "%d", i);
            ImGui::GetWindowDrawList()->AddText(ImVec2(x - 5.0f, pos.y + gauge_height * 0.35f), ImGui::GetColorU32(ImGuiCol_Text), text_buffer);
        }
    }

    // Draw the red zone
    ImVec2 red_zone_min = ImVec2((red_zone - min_level) / (max_level - min_level) * gauge_width + pos.x, pos.y);
    ImVec2 red_zone_max = ImVec2(gauge_max.x, gauge_max.y);
    ImGui::GetWindowDrawList()->AddRectFilled(red_zone_min, red_zone_max, IM_COL32(255, 0, 0, 128));

    // Draw the solid gray bar
    ImVec2 level_max = ImVec2((level - min_level) / (max_level - min_level) * gauge_width + pos.x, gauge_max.y);
    ImGui::GetWindowDrawList()->AddRectFilled(gauge_min, level_max, IM_COL32(160, 160, 160, 128));

    // Draw the peak level
    ImVec2 peak_pos = ImVec2((peak - min_level) / (max_level - min_level) * gauge_width + pos.x, pos.y);
    ImGui::GetWindowDrawList()->AddLine(peak_pos, ImVec2(peak_pos.x, pos.y + gauge_height), IM_COL32(255, 255, 255, 255), 2.0f);

    // Increment the cursor position
    ImRect bb(gauge_min, gauge_max);
    ImGui::ItemSize(gauge_max - gauge_min, 0.0f);
    ImGui::ItemAdd(bb, 0);

    //    ImGui::SetCursorScreenPos(ImVec2(pos.x, pos.y + gauge_height));
}


void QSOPanel::drawHistogram() {
    if (!currentFFTBuffer || audioInToFFT->receivedSamplesCount.load() == 0) {
        ImGui::TextColored(ImVec4(1.0f, 0, 0, 1.0f), "%s", "No microphone! Please");
        ImGui::TextColored(ImVec4(1.0f, 0, 0, 1.0f), "%s", "allow mic access in");
        ImGui::TextColored(ImVec4(1.0f, 0, 0, 1.0f), "%s", "the app permissions!");
        ImGui::TextColored(ImVec4(1.0f, 0, 0, 1.0f), "%s", "(and then restart)");
    }
    else {
        if (submode == "LSB" || submode == "USB") {
            auto mx0 = npmax(currentFFTBuffer);
            auto data = npsqrt(currentFFTBuffer);
            auto mx = npmax(data);
            // only 1/4 of spectrum
            ImVec2 space = ImGui::GetContentRegionAvail();
            space.y = 60; // fft height
            ImGui::PlotHistogram("##fft", data->data(), currentFFTBuffer->size() / 4, 0, NULL, 0,
                                 mx,
                                 space);
            //            if (ImGui::SliderFloat("##_radio_mic_gain_", &this->micGain, 0, +22, "%.1f dB")) {
            //                //
            //            }
            //            ImGui::SameLine();
            //            float db = configPanel->rawInDecibels.getMax(10);
            //            draw_db_gauge(ImGui::GetContentRegionAvail().x, db, 0, -60, +20, 0);
            //            ImGui::Text("Mic:%.3f", configPanel->rawInDecibels.getAvg(1));
        }
        if (submode == "CWU" || submode == "CWL" || submode == "CW") {
            ImGui::Text("CW Mode - use paddle");
        }
    }
}

void QSOPanel::draw(float _currentFreq, ImGui::WaterfallVFO*) {
    this->currentFreq = _currentFreq;
    currentFFTBufferMutex.lock();
    this->drawHistogram();
    float mx = 10 * log10(this->maxSignal.load());
    if (mx > maxTxSignalPeak || maxTxSignalPeakTime < currentTimeMillis() - 500) {
        maxTxSignalPeak = mx;
        maxTxSignalPeakTime = currentTimeMillis();
        ;
    }

    if (maxTxSignalPeak > 0) {
        ImGui::PushStyleColor(ImGuiCol_Text, ImVec4(1.0f, 0, 0, 1.0f));
    }
    ImGui::Text("TX IQ: ");
    if (maxTxSignalPeak > 0) {
        ImGui::PopStyleColor();
    }
    ImGui::SameLine();
    draw_db_gauge(ImGui::GetContentRegionAvail().x, mx, maxTxSignalPeak, -60, +20, 0);
    if (sigpath::transmitter) {

        static int configRead = 0;
        if (!configRead) {
            int hwGain = -1;
            getConfig("trx_txHardwareGain", hwGain);
            if (hwGain != -1) {
                if (sigpath::transmitter) {
                    sigpath::transmitter->setTransmitHardwareGain(hwGain);
                }
            }
            configRead = 1;
        }

        //        if (ImGui::Checkbox("PostPro(SSB)", &this->postprocess)) {
        //        }
        if (false) {
            int fillLevel = (int)sigpath::transmitter->getFillLevel();
            if (fillLevel <= 1 && sigpath::transmitter->getTXStatus()) {
                ImGui::PushStyleColor(ImGuiCol_Text, ImVec4(1.0f, 0, 0, 1.0f));
            }
            ImGui::Text("TX Queue Size: %d", fillLevel);
            if (fillLevel <= 1 && sigpath::transmitter->getTXStatus()) {
                ImGui::PopStyleColor();
            }
        }
        ImGui::Checkbox("PA enable |", &this->enablePA);
        sigpath::transmitter->setPAEnabled(this->enablePA);
        float swr = sigpath::transmitter->getTransmitSWR();
        if (swr >= 9.9) swr = 9.9; // just not to jump much
        lastSWR.emplace_back(swr);
        lastForward.emplace_back(sigpath::transmitter->getTransmitPower());
        lastReflected.emplace_back(sigpath::transmitter->getReflectedPower());
        ImGui::SameLine();
        ImGui::Text("SWR: %.1f", rtmax(lastSWR));
        ImGui::SameLine();
        ImGui::Text("REF PWR: %.1f", rtmax(lastReflected));

        ImGui::Text("FWD PWR: %.1f", rtmax(lastForward)); // below 10w will - not jump.
        ImGui::SameLine();
        draw_db_gauge(ImGui::GetContentRegionAvail().x, rtmax(lastForward), 0, 0, 10, 5, 1);

        ImGui::Text("TX Soft PA:");
        ImGui::SameLine();
        if (ImGui::SliderInt("##_radio_tx_gain_", &this->txGain, 0, 255)) {
            sigpath::transmitter->setTransmitSoftwareGain(this->txGain);
            setConfig("trx_txGain", this->txGain);
        }
        ImGui::LeftLabel("TX Hard PA:");
        ImGui::SameLine();
        int hwgain = sigpath::transmitter->getTransmitHardwareGain();
        if (ImGui::SliderInt("##_radio_tx_hgain_", &hwgain, 0, 255)) {
            sigpath::transmitter->setTransmitHardwareGain(hwgain);
            setConfig("trx_txHardwareGain", hwgain);
        }
        //        ImGui::LeftLabel("Buffer Latency:");
        //        int latency = sigpath::transmitter->getTransmittedBufferLatency();
        //        if (ImGui::SliderInt("##_tx_buf_latency_", &latency, 0, 96)) {
        //            sigpath::transmitter->setTransmittedBufferLatency(latency);
        //            setConfig("trx_txBufferLatency", latency);
        //        }
        //        int ptthang = sigpath::transmitter->getTransmittedPttDelay();
        //        if (ImGui::SliderInt("##_tx_ptthang_", &ptthang, 0, 96)) {
        //            sigpath::transmitter->setTransmittedPttDelay(ptthang);
        //            setConfig("trx_txPTTHangTime", ptthang);
        //        }
    }
    else {
        ImGui::TextColored(ImVec4(1.0f, 0, 0, 1.0f), "%s", "Transmitter not playng");
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
QSOPanel::~QSOPanel() {
    stopAudioPipeline();
}
void QSOPanel::init() {
    getConfig("trx_txGain", this->txGain);
}


void ConfigPanel::draw(ImGui::WaterfallVFO* vfo) {
    gui::mainWindow.qsoPanel->currentFFTBufferMutex.lock();
    gui::mainWindow.qsoPanel->drawHistogram();
    gui::mainWindow.qsoPanel->currentFFTBufferMutex.unlock();
    ImVec2 space = ImGui::GetContentRegionAvail();

    ImGui::LeftLabel("Audio lo cutoff frequency");
    if (ImGui::SliderInt("##lowcutoff", &highPass, 0, lowPass, "%d Hz")) {
        setConfig("trx_highPass", highPass);
    }
    draw_db_gauge(space.x, highPassDecibels.getMax(3), highPassDecibels.getPeak(), -80, +20);


    if (ImGui::Checkbox("DX Equalizer / Compressor##equalizeit", &doEqualize)) {
        setConfig("trx_doEqualize", doEqualize);
    }

    ImGui::SameLine();
    ImGui::FillWidth();
    if (ImGui::SliderFloat("##equalizeit-pa", &compAmp, -60.0f, 60.0f, "%.3f dB PostAmp")) {
        setConfig("trx_compAmp", compAmp);
    }
    //    ImGui::SameLine();
    //    ImGui::LeftLabel("Freq Maim");
    //    if (ImGui::Checkbox("##freqmaim", &doFreqMaim)) {
    //    }
    draw_db_gauge(space.x, equalizerDecibels.getMax(3), equalizerDecibels.getPeak(), -80, +20);

    if (ImGui::Checkbox("Hiss add##_his_add_", &hissAdd)) {
    }


    ImGui::LeftLabel("AGC Attack");
    if (ImGui::SliderFloat("##attack-rec", &agcAttack, 1.0f, 200.0f)) {
        setConfig("trx_agcAttack", agcAttack);
        agc.setAttack(agcAttack / trxAudioSampleRate);
        //        agc2.setAttack(agcAttack);
    }
    ImGui::LeftLabel("AGC Decay");
    if (ImGui::SliderFloat("##decay-rec", &agcDecay, 0.1f, 20.0f)) {
        setConfig("trx_agcDecay", agcDecay);
        agc.setDecay(agcDecay / trxAudioSampleRate);
        //        agc2.setDecay(agcDecay);
    }
    draw_db_gauge(space.x, agcDecibels.getMax(3), agcDecibels.getPeak(), -80, +20);

    //    ImGui::SameLine();
    //    ImGui::LeftLabel("Freq Maim");
    //    if (ImGui::Checkbox("##freqmaim", &doFreqMaim)) {
    //    }

    if (afnr) {
        if (ImGui::Checkbox("Mic NR##donr", &afnr->allowed)) {
            setConfig("trx_afnrAllowd", afnr->allowed);
        }
        ImGui::SameLine();
        ImGui::FillWidth();
        if (ImGui::SliderFloat("##nr-preamp", &afnr->preAmpGain, -60.0f, 60.0f, "%.3f dB PreAmp")) {
            setConfig("trx_afnrPreAmpGain", afnr->preAmpGain);
        }
    }
    draw_db_gauge(space.x, nrDecibels.getMax(3), nrDecibels.getPeak(), -80, +20);
    if (ImGui::Checkbox("Mic SQL##donrsql", &micSql)) {
        setConfig("trx_SqlAllowd", micSql);
    }
    ImGui::SameLine();
    if (ImGui::SliderFloat("##mic-sql", &micSqlLevel, -60.0f, 0.0f, "%.3f dB")) {
        setConfig("trx_SqlLevel", micSqlLevel);
    }
    if (vfo) {
        lowPass = vfo->bandwidth;
    }
    else {
        lowPass = 2700;
    }
    ImGui::Text("Audio bandwidth (high cut-off): %d", lowPass);
    //    if (ImGui::SliderInt("##highcutoff", &lowPass, highPass, 12000, "%d Hz")) {
    //        setConfig("trx_lowPass", lowPass);
    //    }
    //    ImGui::SameLine();
    //    if (ImGui::Button("Reset NR")) {
    //        afnr.reset();
    //    }

    draw_db_gauge(space.x, outDecibels.getMax(3), outDecibels.getPeak(), -80, +20);

    ImGui::SetCursorPos(ImGui::GetCursorPos() + ImVec2(0, style::baseFont->FontSize * 0.4));

    switch (recorder.mode) {
    case SimpleRecorder::RECORDING_IDLE:
        if (doFingerButton("Record")) {
            recorder.startRecording();
        }
        if (!recorder.data.empty()) {
            ImGui::SameLine();
            if (doFingerButton("<------- Play")) {
                recorder.startPlaying();
            }
        }
        break;
    case SimpleRecorder::RECORDING_STARTED:
        if (doFingerButton("Stop Rec")) {
            recorder.stop();
        }
        break;
    case SimpleRecorder::PLAYING_STARTED:
        ImGui::BeginDisabled(true);
        if (doFingerButton("Record")) {
        }
        ImGui::EndDisabled();
        ImGui::SameLine();
        if (doFingerButton("<-------- Stop Play")) {
            recorder.stop();
        }
        break;
    }
}
