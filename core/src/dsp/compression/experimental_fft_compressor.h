#pragma once
#include "../processor.h"
#include "pcm_type.h"
#include "utils/arrays.h"
#include "dsp/window/blackman.h"
#include <cmath>


namespace dsp::compression {

    using namespace ::dsp::arrays;

    class ExperimentalFFTCompressor : public Processor<complex_t, complex_t> {
        using base_type = Processor<complex_t, complex_t>;
    public:

        int sampleRate = 0;
        int sliceMsec = 50;
        int fftSize = 1024;
        double noiseMultiplier = 1.0;

        Arg<FFTPlan> fftPlan;
        std::shared_ptr<std::vector<complex_t>> inArray;
        std::vector<float> fftWindow;

        std::vector<complex_t> inputBuffer;

        const int minRecents = 10;
        std::vector<ComplexArray> recents;
        std::vector<FloatArray> recentsSpectrum;

        ExperimentalFFTCompressor() {}

        ExperimentalFFTCompressor(stream<complex_t>* in) { init(in); }

        void setSampleRate(int sampleRate) {
            if (sampleRate != this->sampleRate) {
                clear();
                this->sampleRate = sampleRate;
                fftSize = sampleRate * sliceMsec / 1000;
                fftSize = pow(2, floor(log2(fftSize)));
                fftPlan = allocateFFTWPlan(false, fftSize);
                inArray = std::make_shared<std::vector<dsp::complex_t>>(fftSize);

                fftWindow.resize(fftSize);
                for (int i = 0; i < fftSize; i++) { fftWindow[i] = dsp::window::blackman(i, fftSize); }

            }
        }

        void clear() {
            inputBuffer.clear();
        }

        bool enabled = false;

        int signalWidth = 300;

        void setEnabled(bool enabled) {
            this->enabled = enabled;
        }

        bool isEnabled() {
            return enabled;
        }
        
        int iteration = 0;

        const int noiseNPoints = 16;
        std::vector<dsp::complex_t> noiseFigure; // mean, variance, mean, variance, mean, variance..
        std::mutex noiseFigureLock;

        inline  int process() {

            if (inputBuffer.size() < fftSize) {
                return 0;
            }

            inArray->clear();
            inArray->insert(inArray->begin(), inputBuffer.begin(), inputBuffer.begin() + fftSize);
            inputBuffer.erase(inputBuffer.begin(), inputBuffer.begin() + fftSize);
            // attempt
            auto out = fftPlan->npfftfft(clone(inArray));
            swapfft(out);
            recents.emplace_back(clone(out));

            volk_32fc_32f_multiply_32fc((lv_32fc_t*)inArray->data(), (lv_32fc_t*)inArray->data(), fftWindow.data(), fftSize);
            auto spectrumOut = fftPlan->npfftfft(inArray);
            swapfft(spectrumOut);
            auto spectrum = npzeros(fftSize);
            volk_32fc_s32f_power_spectrum_32f(spectrum->data(), (const lv_32fc_t*)spectrumOut->data(), fftSize, fftSize);


            float hzTick = ((float)sampleRate) / fftSize;
            int smallTick = signalWidth / hzTick;
            int largeTick = smallTick * 10;

            auto csma = centeredSma(spectrum, largeTick);
            spectrum = subeach(spectrum, csma);

            recentsSpectrum.emplace_back(spectrum);

            if (recents.size() < minRecents) {
                return 0;
            }

            std::copy(recents[0]->begin(), recents[0]->end(), this->out.writeBuf);


            spectrum = npzeros(fftSize);
            for(int r=0; r<recentsSpectrum.size(); r++) {
                spectrum = addeach(spectrum, recentsSpectrum[r]);
            }
            spectrum = div(spectrum, recentsSpectrum.size());

            // averaged spectrum ready, normalized around 0. Calculating noise floor

            auto smallSma = centeredSma(spectrum, smallTick);
            auto largeSma = centeredSma(spectrum, largeTick);
            auto mask = npzeros(fftSize);
            int rawNoiseCount = 0;
            auto sdata = spectrum->data();;
            auto sigmaskdata = mask->data();
            for(int i=1; i<fftSize-1; i++) {
                if (sdata[i] * sdata[i-1] < 0 && sdata[i] * sdata[i+1] < 0) {
                    // noikse
                    rawNoiseCount++;
                } else {
                    sigmaskdata[i] = 1.0;
                }
            }
            for(int i=0; i<fftSize-2; i++) {
                if (sigmaskdata[i] == 0 && sigmaskdata[i+2] == 0 && sigmaskdata[i+1] != 0) {
                    sigmaskdata[i+1] = 0;       // remove singular signals, which are not.
                    rawNoiseCount++;
                }
            }

            int noiseCount = 0;
            int sliceSize = fftSize / noiseNPoints;

            if (rawNoiseCount > 30 * noiseNPoints) {
                auto spectrumVars = npzeros(noiseNPoints);  // variances
                for (int p = 0; p < noiseNPoints; p++) {
                    float sum = 0;
                    int cnt = 0;
                    for (int q = 0; q < sliceSize; q++) {
                        if (sigmaskdata[q + p * sliceSize] == 0) {
                            double dbl = sdata[q + p * sliceSize];
                            sum += dbl * dbl;
                            cnt++;
                        }
                    }
                    if (cnt == 0) {
                        cnt = 1; // value will be 0
                    }
                    spectrumVars->at(p) = sum / cnt;
                }

                math::linearInterpolateHoles(spectrumVars->data(), spectrumVars->size());
                spectrumVars->emplace_back(spectrumVars->back()); // duplicate last item.
                spectrumVars = centeredSma(spectrumVars, 5);


                // build noise figure.

                auto iMean = npzeros(noiseNPoints);
                auto qMean = npzeros(noiseNPoints);
                auto iDev = npzeros(noiseNPoints);
                auto qDev = npzeros(noiseNPoints);
                char logg[10000] = {0};
                for (int p = 0; p < noiseNPoints; p++) {
                    dsp::complex_t iqSum = {0, 0};
                    int cnt = 0;
                    for (int q = 0; q < sliceSize; q++) {
                        int srcIndex = q + p * sliceSize;
                        if (smallSma->at(srcIndex) < spectrumVars->at(p) * noiseMultiplier && noiseMultiplier > 0) {
                            iqSum += this->out.writeBuf[srcIndex];
                            cnt++;
                        }
                    }
                    sprintf(logg + strlen(logg), "%d: cnt1=%d", p, cnt);
                    if (cnt == 0) {
                        cnt = 1;
                    }
                    dsp::complex_t mean = {iqSum.re / cnt, iqSum.im / cnt};
                    iMean->at(p) = mean.re;
                    qMean->at(p) = mean.im;

                    dsp::complex_t iqVar = {0, 0};
                    cnt = 0;
                    for (int q = 0; q < sliceSize; q++) {
                        int srcIndex = q + p * sliceSize;
                        if (smallSma->at(srcIndex) < spectrumVars->at(p) * noiseMultiplier && noiseMultiplier > 0) {
                            auto dd = this->out.writeBuf[q + p * sliceSize] - mean;
                            iqVar += {dd.re * dd.re, dd.im * dd.im};
                            cnt++;
                        }
                    }
                    sprintf(logg + strlen(logg), " cnt2=%d iqv=%f+i*%f .", cnt, iqVar.re, iqVar.im);
                    if (cnt == 0) {
                        cnt = 1;
                    }
                    dsp::complex_t iqDev = {sqrt(iqVar.re / cnt), sqrt(iqVar.im / cnt)};
                    iDev->at(p) = iqDev.re;
                    qDev->at(p) = iqDev.im;
                }
//                flog::info("comp: {}", logg);
                math::linearInterpolateHoles(iMean->data(), iMean->size());
                math::linearInterpolateHoles(qMean->data(), qMean->size());
                math::linearInterpolateHoles(iDev->data(), iDev->size());
                math::linearInterpolateHoles(qDev->data(), qDev->size());
                iMean = centeredSma(iMean, 5);
                qMean = centeredSma(qMean, 5);
                iDev = centeredSma(iDev, 5);
                qDev = centeredSma(qDev, 5);

                if (noiseMultiplier > 0) {
                    noiseFigureLock.lock();
                    noiseFigure.clear();
                    for (int p = 0; p < noiseNPoints; p++) {
                        dsp::complex_t iqMean = {iMean->at(p), qMean->at(p)};
                        dsp::complex_t iqDev = {iDev->at(p), qDev->at(p)};
                        if (isnan(iqMean.im) || isnan(iqMean.re) || isnan(iqDev.im) || isnan(iqDev.re)) {
                            flog::info("noise figure is nan: {} {} {} {}", iqMean.im, iqMean.re, iqDev.im, iqDev.re);
                        }
                        noiseFigure.emplace_back(iqMean);
                        noiseFigure.emplace_back(iqDev);
                    }
                    noiseFigureLock.unlock();
                }

                for(int i=0; i<fftSize; i++) {
                    if (smallSma->at(i) < spectrumVars->at(i / sliceSize) * noiseMultiplier && noiseMultiplier > 0) {
                        // remove signals below the noise level.
                        this->out.writeBuf[i] = {0, 0};
                        noiseCount++;
                    }
                }

            }


//            flog::info("RawNoiseCount={} NoiseCount={} ", rawNoiseCount, noiseCount);

//            iteration++;
//            if (iteration == 50) {
//                FILE *t = fopen("/tmp/arr.bin","wb");
//                if (t) {
//                    for(int r=0; r<minRecents; r++) {
//                        fwrite(recents[r].get()->data(), sizeof(dsp::complex_t), fftSize, t);
//                    }
//                    fclose(t);
//                }
//            }

            recents.erase(recents.begin() + 0);
            if (recentsSpectrum.size() > minRecents * 2) {
                recentsSpectrum.erase(recentsSpectrum.begin() + 0);
            }

            return fftSize;
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

                inputBuffer.insert(inputBuffer.end(), base_type::_in->readBuf, base_type::_in->readBuf + count);
                base_type::_in->flush();

                int outCount = process();

                // Swap if some data was generated
                if (outCount) {
                    if (!base_type::out.swap(outCount)) { return -1; }
                }
//                flog::info("experimental FFT compr: inCount={} outCount={} bufSize={}", count, outCount, (int)inputBuffer.size());
                return outCount;
            }
        }

    };
}