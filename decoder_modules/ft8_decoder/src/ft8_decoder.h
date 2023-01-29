#pragma once

extern int four2a_d2c_cnt;

#include "ft8_etc/decoderms.h"
#include "ft8_etc/mscore.h"

namespace dsp {

    struct FT8Decoder {



    };

    namespace ft8 {
        enum {
            DMS_FT8 = 11
        } DecoderMSMode;


        // input stereo samples, nsamples (number of pairs of float)
        inline void decodeFT8(int sampleRate, dsp::stereo_t* samples, long long nsamples, std::function<void(int mode, QStringList result)> callback) {
            //
            //
            //

            four2a_d2c_cnt = 0;

            std::vector<dsp::stereo_t> resampledV;

            if (sampleRate != 12000) {
                long long int outSize = 3 * (nsamples * 12000) / sampleRate;
                resampledV.resize(outSize);
                dsp::multirate::RationalResampler<dsp::stereo_t> res;
                res.init(nullptr, sampleRate, 12000);
                nsamples = res.process(nsamples, samples, resampledV.data());
                samples = resampledV.data();
            }


            // this needs 44100 sample rate
            std::vector<short> converted;
            converted.reserve(nsamples);
            for (int q = 0; q < nsamples; q++) {
                converted.emplace_back(samples[q].l * 16383.52);
            }

            //    auto core = std::make_shared<MsCore>();
            //    core->ResampleAndFilter(converted.data(), converted.size());
            auto dms = std::make_shared<DecoderMs>();
            dms->setMode(DMS_FT8);
            {
                QStringList ql;
                ql << "CALL";
                ql << "CALL";
                dms->SetWords(ql, 0, 0);
            }
            {
                QStringList ql;
                ql << "CALL";
                ql << "";
                ql << "";
                ql << "";
                ql << "";
                dms->SetCalsHash(ql);
            }
            dms->SetResultsCallback(callback);
            dms->SetDecode(converted.data(), converted.size(), "120000", 0, 4, false, true, false);
            while (dms->IsWorking()) {
                usleep(100000);
            }
            return;
        }

    }

}