#pragma once

#include <core.h>
#include <../../misc_modules/recorder/src/wav.h>
#include "symbolic.h"

#ifndef _WIN32
#include <wait.h>
#endif

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

            static std::atomic_int _seq = 100;
            int seq = ++_seq;
            std::string seqS = std::to_string(seq);
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
            if (max > 1) {
                for(int i=0; i<nsamples; i++) {
                    samples[i].l /= max;
                    samples[i].r /= max;
                }
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
            w.setSampleType(wav::SAMP_TYPE_FLOAT32);
            w.setSamplerate(12000);
            auto wavPath = "/tmp/sdrpp_ft8_mshv.wav." + seqS;
            w.open(wavPath);
            w.write((float*)samples, nsamples);
            w.close();
            std::string modulesDir = core::configManager.conf["modulesDirectory"]; // xxx/lib/sdrpp/plugins
            auto binPath = modulesDir+"/../../../bin";
            auto decoderPath = binPath+"/sdrpp_ft8_mshv";
            spdlog::info("FT8 Decoder: executing: {}", decoderPath);
            auto outPath= "/tmp/sdrpp_ft8_mshv.out."+seqS;
            auto errPath= "/tmp/sdrpp_ft8_mshv.err."+seqS;

#ifdef _WIN32
#else
            int f = fork();
            if (f == 0) {

                close(0);
                close(1);
                close(2);
                open("/dev/null", O_RDONLY, 0600); // input
                open(outPath.c_str(), O_CREAT|O_TRUNC|O_WRONLY, 0600); // out
                open(errPath.c_str(), O_CREAT|O_TRUNC|O_WRONLY, 0600); // err
                auto err = execl(decoderPath.c_str(), decoderPath.c_str(), "--decode", wavPath.c_str(), NULL);
                if (err < 0) {
                    perror("exec: ");
                }
                close(0);
                close(1);
                close(2);
                exit(0);
            }
            else {
                int nsent = 0;
                while(true) {
                    auto finished = waitpid(f, NULL, WNOHANG);
                    usleep(100000);
                    auto hdl = open(outPath.c_str(), O_RDONLY);
                    char rdbuf[10000];
                    if (hdl > 0) {
                        int nrd = read(hdl, rdbuf, sizeof(rdbuf)-1);
                        if (nrd > 0) {
                            rdbuf[10000 - 1] = 0;
                            rdbuf[nrd] = 0;
                            std::vector<std::string> thisResult;
                            splitString(rdbuf, '\n', [&](const std::string &p){
                                if (p.find("FT8_OUT") == 0) {
                                    thisResult.emplace_back(p);
                                }
                            });
                            for(int q=nsent; q<thisResult.size(); q++) {
                                std::vector<std::string> singleBroken;
                                splitString(thisResult[q], '\t', [&](const std::string &p){
                                    singleBroken.emplace_back(p);
                                });
                                std::vector<std::string> selected;
                                //FT8_OUT	1675635874870	30	{0}	120000	{1}	-19	{2}	0.2	{3}	775	{4}	SQ9KWU DL1PP -14	{5}	? 0	{6}	0.1	{7}	1975
                                if (singleBroken.size() > 18) {
                                    selected.emplace_back(singleBroken[4]);
                                    selected.emplace_back(singleBroken[6]);
                                    selected.emplace_back(singleBroken[8]);
                                    selected.emplace_back(singleBroken[10]);
                                    selected.emplace_back(singleBroken[12]);
                                    selected.emplace_back(singleBroken[14]);
                                    selected.emplace_back(singleBroken[16]);
                                    selected.emplace_back(singleBroken[18]);
                                }
                                callback(DMS_FT8, selected);
                            }
                            nsent = thisResult.size();
                        }
                        close(hdl);
                    }
                    if (finished != 0) {
                        break;
                    }

                }
//                unlink(wavPath.c_str());
                spdlog::info("FT8 Decoder: process ended.");
            }
#endif


            return;
        }
    }

}

