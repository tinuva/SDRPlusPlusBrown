#pragma once

#include <core.h>
#include <utils/wav.h>
#include <utils/riff.h>
#include "symbolic.h"

#ifndef _WIN32

#include <wait.h>

#endif

// extern int four2a_d2c_cnt;
extern void
doDecode(const char *path, std::function<void(int mode, std::vector<std::string> result)> callback);

namespace dsp {

    struct FT8Decoder {

    };

    namespace ft8 {


        enum {
            DMS_FT8 = 11
        } DecoderMSMode;

        void invokeDecoder(const std::string &wavPath, const std::string &outPath,
                           const std::string &errPath, std::function<void(int mode,
                                                                          std::vector<std::string> result)> callback) {

#ifdef __ANDROID__
            Dl_info info;
            dladdr((void *) &invokeDecoder, &info);
            auto path = std::string(info.dli_fname);
            path = path.substr(0, path.rfind('/'));
            auto decoderPath = path + "/sdrpp_ft8_mshv.so";
#else
            core::configManager.acquire();
            std::string modules = core::configManager.conf["modulesDirectory"];
            core::configManager.release(false);
            auto decoderPath = modules+"/../../sdrpp_ft8_mshv";
#endif

            int f = fork();
            bool forceDebugEnter = false;
            if (forceDebugEnter || !f) {

                if (!forceDebugEnter) {
                    close(0);
                    close(1);
                    close(2);
                    open("/dev/null", O_RDONLY, 0600); // input
                    open(outPath.c_str(), O_CREAT | O_TRUNC | O_WRONLY, 0600); // out
                    open(errPath.c_str(), O_CREAT | O_TRUNC | O_WRONLY, 0600); // err
                }
                write(1, "sample output here 0\n", strlen("sample output here 0\n"));
                fprintf(stdout, "decoderPath=%s\n", decoderPath.c_str());
                fprintf(stdout, "ctm=%lld\n", currentTimeMillis());
                fflush(stdout);
#if 1

                spdlog::info("FT8 Decoder: executing: {}", decoderPath);
                auto err = execl(decoderPath.c_str(), decoderPath.c_str(), "--decode",
                                 wavPath.c_str(), NULL);
                static auto q = errno;
                spdlog::info("FT8 Decoder: executing: {}: error={}", decoderPath, q);
                if (err < 0) {
                    perror("exec: ");
                }
#else
                doDecode(wavPath.c_str(), [&](int mode, std::vector<std::string> result) {
//                    callback(mode, result);
                });
#endif
                write(1, "\nBefore process exit\n", strlen("\nBefore process exit\n"));
                close(0);
                close(1);
                close(2);
                abort();     // exit does not terminate well.
                //                exit(0);
            } else {
                int nsent = 0;
                int count = 0;
                while (true) {
                    usleep(100000);
                    auto hdl = open(outPath.c_str(), O_RDONLY);
                    char rdbuf[10000];
                    if (hdl > 0) {
                        int nrd = read(hdl, rdbuf, sizeof(rdbuf) - 1);
                        if (nrd > 0) {
                            rdbuf[10000 - 1] = 0;
                            rdbuf[nrd] = 0;
                            std::vector<std::string> thisResult;
                            splitString(rdbuf, "\n", [&](const std::string &p) {
                                if (p.find("FT8_OUT") == 0) {
                                    thisResult.emplace_back(p);
                                }
                            });
                            for (int q = nsent; q < thisResult.size(); q++) {
                                std::vector<std::string> singleBroken;
                                splitStringV(thisResult[q], "\t", singleBroken);
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
                                count++;
                            }
                            nsent = thisResult.size();
                        }
                        close(hdl);
                    }
                    auto finished = waitpid(f, NULL, WNOHANG);
                    if (finished != 0) {
                        break;
                    }

                }
                //                unlink(wavPath.c_str());
                spdlog::info("FT8 Decoder: process ended. Count messages: {}", count);
            }

        }

        inline void decodeFT8(int sampleRate, dsp::stereo_t *samples, long long nsamples,
                              std::function<void(int mode,
                                                 std::vector<std::string> result)> callback) {

            static std::atomic_int _seq = 100;
            int seq = ++_seq;
            std::string seqS = std::to_string(seq);
            //
            //
            //

            std::string tempPath;
            core::configManager.acquire();
            if (core::configManager.conf.find("tempDir") != core::configManager.conf.end()) {
                tempPath = core::configManager.conf["tempDir"];
            }
            core::configManager.release(false);

            if (tempPath.empty()) {
                tempPath = (std::string) core::args["temp"];
            }

            if (tempPath.empty()) {
                std::error_code ec;
                auto p = std::filesystem::temp_directory_path(ec);
                if (!ec.value()) {
                    tempPath = std::string(p.c_str());
                } else {
                    tempPath = "/tmp";
                }
            }


            std::vector<dsp::stereo_t> samplez;
            samplez.resize(nsamples);
            std::vector<dsp::stereo_t> resampledV;

            int max = 0;
            for (int i = 0; i < nsamples; i++) {
                max = std::max<float>(abs(samples[i].l), max);
                max = std::max<float>(abs(samples[i].r), max);
            }

            spdlog::info("max: {}", max);
            if (max == 0) {
                max = 1;        // leave zeros. whatever.
            }
            for (int i = 0; i < nsamples; i++) {
                samplez[i].l = samples[i].l / max;
                samplez[i].r = samples[i].r / max;
            }
            samples = samplez.data();

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
            auto wavPath = tempPath + "/sdrpp_ft8_mshv.wav." + seqS;
            w.open(wavPath);
            w.write((float *) samples, nsamples);
            w.close();


#ifdef _WIN32
#else

            auto outPath = tempPath + "/sdrpp_ft8_mshv.out." + seqS;
            auto errPath = tempPath + "/sdrpp_ft8_mshv.err." + seqS;

            invokeDecoder(wavPath, outPath, errPath, callback);

#endif


            return;
        }
    }

}

