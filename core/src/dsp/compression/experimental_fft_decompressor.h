#pragma once
#include "../processor.h"
#include "pcm_type.h"
#include "utils/arrays.h"
#include <cmath>
#include <random>



namespace dsp::compression {

    using namespace ::dsp::arrays;

    class ExperimentalFFTDeCompressor : public Processor<complex_t, complex_t> {
        using base_type = Processor<complex_t, complex_t>;
    public:

        int fftSize = 0;
        Arg<FFTPlan> fftPlan;
        ComplexArray inArray;

        std::random_device rd;
        std::mt19937 rgen;
        float noiseMultiplierDB = 0.0;

        ExperimentalFFTDeCompressor() : Processor<complex_t, complex_t>(), rgen(rd()) {

        }

        //ExperimentalFFTDeCompressor(stream<complex_t>* in) { init(in); }

        void setFFTSize(int fftSize) {
            if (this->fftSize != fftSize) {
                this->fftSize = fftSize;
                fftPlan = allocateFFTWPlan(true, fftSize);
                inArray = npzeros_c(fftSize);
            }
        }

        bool enabled = false;

        void setEnabled(bool enabled) {
            this->enabled = enabled;
        }

        bool isEnabled() {
            return enabled;
        }

        float db_to_linear(float decibels) {
            float linear_scale = std::pow(10, decibels / 20);
            return linear_scale;
        }

        int run() {
            int count = base_type::_in->read();
            if (count < 0) { return -1; }

            if (!enabled) {
                std::copy(_in->readBuf, _in->readBuf + count, this->out.writeBuf);
                base_type::_in->flush();
                if (count > 0) {
                    if (!base_type::out.swap(count)) { return -1; }
                }
                return count;
            } else {

                setFFTSize(count); // always right size.
                inArray->clear();
                inArray->insert(inArray->begin(), _in->readBuf, _in->readBuf + count);
                base_type::_in->flush();

                if (noiseFigure.size() > 0) {
                    int nparts = noiseFigure.size() / 2;
                    auto indata = inArray->data();
                    int singlePart = fftSize / nparts;

                    std::vector<std::normal_distribution<float>> distre(nparts+1, std::normal_distribution<float>(0, 1));
                    std::vector<std::normal_distribution<float>> distim(nparts+1, std::normal_distribution<float>(0, 1));
                    auto  noiseMult = db_to_linear(noiseMultiplierDB);
                    for (int i = 0; i < nparts; i++) {
                        distre[i] = std::normal_distribution<float>(noiseFigure[i * 2 + 0].re, noiseMult * noiseFigure[i * 2 + 1].re);
                        distim[i] = std::normal_distribution<float>(noiseFigure[i * 2 + 0].im, noiseMult * noiseFigure[i * 2 + 1].im);
                    }
                    distre[nparts] = distre[nparts-1];
                    distim[nparts] = distim[nparts-1];

                    for (int i = 0; i < fftSize; i++) {
                        if (indata[i].re == 0 && indata[i].im == 0) {
                            indata[i].re = distre[i/singlePart](rgen);
                            indata[i].im = distim[i/singlePart](rgen);
                        }
                    }
                }


                swapfft(inArray); // was swapped
                auto out = fftPlan->npfftfft(inArray);
                std::copy(out->begin(), out->begin() + fftSize, this->out.writeBuf);
                if (!base_type::out.swap(fftSize)) { return -1; }
                //flog::info("experimental FFT decompressor: inCount={} outCount={}", count, fftSize);
                return fftSize;
            }
        }

        std::vector<dsp::complex_t> noiseFigure;

        float lowpass(float oldd, float neww) {
            float delay = 0.8;
            return oldd * delay + neww * (1-delay);
        }

        void setNoiseFigure(std::vector<dsp::complex_t> figure) {
            if (noiseFigure.size() != figure.size()) {
                noiseFigure = figure;
            } else {
                for(int q=0; q<figure.size(); q++) {
                    noiseFigure[q].re = lowpass(noiseFigure[q].re, figure[q].re);
                    noiseFigure[q].im = lowpass(noiseFigure[q].im, figure[q].im);
                }
            }
        }
    };
}