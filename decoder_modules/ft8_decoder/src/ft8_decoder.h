#pragma once

#include <core.h>
#include <../../misc_modules/recorder/src/wav.h>
#include <wait.h>

// extern int four2a_d2c_cnt;

namespace dsp {

    struct FT8Decoder {

    };

    namespace ft8 {
        enum {
            DMS_FT8 = 11
        } DecoderMSMode;


        // input stereo samples, nsamples (number of pairs of float)
        inline void decodeFT8(int sampleRate, dsp::stereo_t* samples, long long nsamples, std::function<void(int mode, std::vector<std::string> result)> callback) {

            //
            //
            //


            std::vector<dsp::stereo_t> resampledV;

            int max = 0;
            for(int i=0; i<nsamples; i++) {
                max = std::max<float>(abs(samples[i].l), max);
                max = std::max<float>(abs(samples[i].r), max);
            }

            spdlog::info("max: {}", max);

            for(int i=0; i<nsamples; i++) {
                samples[i].l /= max;
                samples[i].r /= max;
            }


            if (sampleRate != 12000) {
                long long int outSize = 3 * (nsamples * 12000) / sampleRate;
                resampledV.resize(outSize);
                dsp::multirate::RationalResampler<dsp::stereo_t> res;
                res.init(nullptr, sampleRate, 12000);
                nsamples = res.process(nsamples, samples, resampledV.data());
                samples = resampledV.data();
            }



            wav::Writer w;
            w.setChannels(2);
            w.setFormat(wav::FORMAT_WAV);
            w.setSampleType(wav::SAMP_TYPE_INT16);
            w.setSamplerate(12000);
            w.open("/tmp/ft8_mshv_tmp.wav");
            w.write((float*)samples, nsamples);
            w.close();

#ifdef _WIN32
#else
            int f = fork();
            if (f == 0) {
                close(0);
                close(1);
                close(2);
                open("/tmp/")
                auto err = execl("/tmp/sdrpp/bin/sdrpp_ft8_mshv", "--decode", "/tmp/ft8_mshv_tmp.wav", NULL);
                exit(0);
            }
            else {
                waitpid(f, NULL, 0);
            }
#endif


            return;
        }
    }

}

