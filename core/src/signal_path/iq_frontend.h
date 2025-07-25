#pragma once
#include "../dsp/buffer/frame_buffer.h"
#include "../dsp/buffer/reshaper.h"
#include "../dsp/multirate/power_decimator.h"
#include "../dsp/correction/dc_blocker.h"
#include "../dsp/chain.h"
#include "../dsp/routing/splitter.h"
#include "../dsp/channel/rx_vfo.h"
#include "../dsp/sink/handler_sink.h"
#include "../dsp/processor.h"
#include "../dsp/math/conjugate.h"
#include "../dsp/detector/signal_detector.h"
#include <fftw3.h>
#include "utils/event.h"
#include "utils/arrays.h"
#include <atomic>

class IQFrontEnd {
public:
    ~IQFrontEnd();

    enum FFTWindow {
        RECTANGULAR,
        BLACKMAN,
        NUTTALL,
    };

    void init(dsp::stream<dsp::complex_t>* in, double sampleRate, bool buffering, int decimRatio, bool dcBlocking, int fftSize, double fftRate, FFTWindow fftWindow, float* (*acquireFFTBuffer)(void* ctx), void (*releaseFFTBuffer)(void* ctx), void* fftCtx);

    void setInput(dsp::stream<dsp::complex_t>* in);
    void setSampleRate(double sampleRate);
    inline double getSampleRate() { return _sampleRate / _decimRatio; }

    void setBuffering(bool enabled);
    void setDecimation(int ratio);
    void setInvertIQ(bool enabled);
    void setDCBlocking(bool enabled);


    void addPreprocessor(dsp::Processor<dsp::complex_t, dsp::complex_t>* processor, bool enabled);
    void removePreprocessor(dsp::Processor<dsp::complex_t, dsp::complex_t>* processor);
    void togglePreprocessor(dsp::Processor<dsp::complex_t, dsp::complex_t>* processor, bool enabled);

    void bindIQStream(dsp::stream<dsp::complex_t>* stream);
    void unbindIQStream(dsp::stream<dsp::complex_t>* stream);

    dsp::channel::RxVFO* addVFO(std::string name, double sampleRate, double bandwidth, double offset);
    void removeVFO(std::string name);

    void setFFTSize(int size);
    void setFFTRate(double rate);
    double getFFTRate() {
        return _fftRate;
    }
    void setFFTWindow(FFTWindow fftWindow);

    void flushInputBuffer();

    void start();
    void stop();

    double getEffectiveSamplerate();

    long long getCurrentStreamTime();
    void setCurrentStreamTime(long long x) {
        _currentStreamTime = x;
    }

    Event<double> onEffectiveSampleRateChange;

    std::string operatorCallsign; // callsign assigned to radio.
    std::string operatorLocation; // maidenhead

    int secondsAdjustment = 0;  // adjust for ft8 decode when local time mismatches

    // Signal detector preprocessor
    dsp::detector::SignalDetector detectorPreprocessor;

protected:
    std::atomic<long long> _currentStreamTime = 0; // unix time millis. 0 means realtime, otherwise simulated time.
    static void handler(dsp::complex_t* data, int count, void* ctx);
    void updateFFTPath(bool updateWaterfall = false);

    static inline double genDCBlockRate(double sampleRate) {
        return 50.0 / sampleRate;
    }

    static inline void genReshapeParams(double sampleRate, int size, double rate, int& skip, int& nzSampCount) {
        int fftInterval = round(sampleRate / rate);
        nzSampCount = std::min<int>(fftInterval, size);
        skip = fftInterval - nzSampCount;
    }

    // Input buffer
    dsp::buffer::SampleFrameBuffer<dsp::complex_t> inBuf;

    // Pre-processing chain
    dsp::multirate::PowerDecimator<dsp::complex_t> decim;
    dsp::math::Conjugate conjugate;
    dsp::correction::DCBlocker<dsp::complex_t> dcBlock;
    dsp::chain<dsp::complex_t> preproc;

    // Splitting
    dsp::routing::Splitter<dsp::complex_t> split;

    // FFT
    dsp::stream<dsp::complex_t> fftIn;
    dsp::buffer::Reshaper<dsp::complex_t> reshape;
    dsp::sink::Handler<dsp::complex_t> fftSink;

    // VFOs
    std::map<std::string, dsp::stream<dsp::complex_t>*> vfoStreams;
    std::map<std::string, dsp::channel::RxVFO*> vfos;

    // Parameters
    double _sampleRate;
    double _decimRatio;
    int _fftSize;
    double _fftRate;
    FFTWindow _fftWindow;
    float* (*_acquireFFTBuffer)(void* ctx);
    void (*_releaseFFTBuffer)(void* ctx);
    void* _fftCtx;

    // Processing data
    int _nzFFTSize;
    float* fftWindowBuf;
//    fftwf_complex *fftInBuf, *fftOutBuf;
//    fftwf_plan fftwPlanImplFFTW;
    dsp::arrays::Arg<dsp::arrays::FFTPlan> fftPlan;
    float* fftDbOut;

    double effectiveSr;

    bool _init = false;

};