#include <core.h>
#include <iostream>
#include <stdio.h>
#include "../../misc_modules/recorder/src/wav.h"
#include "dsp/types.h"
#include "dsp/multirate/polyphase_resampler.h"
#include "dsp/multirate/rational_resampler.h"

#include "ft8_etc/mshv_support.h"
#include "ft8_etc/mscore.h"
#include "ft8_etc/decoderms.h"

extern long long currentTimeMillis();


namespace ft8 {
    enum {
        DMS_FT8 = 11
    } DecoderMSMode;


    // input stereo samples, nsamples (number of pairs of float)
    inline void decodeFT8(int sampleRate, dsp::stereo_t* samples, long long nsamples, std::function<void(int mode, QStringList result)> callback) {
        //
        //
        //

//        four2a_d2c_cnt = 0;

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



void doDecode(const char *path) {
    FILE *f = fopen(path,"rb");
    if (!f) {
        fprintf(stderr,"ERROR Cannot open file %s\n", path);
        exit(1);
    }
    fseek(f, 0, SEEK_END);
    long long size = ftell(f);
    fseek(f, 0, SEEK_SET);
    uint8_t *buf = (uint8_t *)malloc(size);
    if (!buf) {
        fprintf(stderr,"ERROR Cannot alloc %lld\n", size);
        exit(1);
    }
    fread((void *)buf, size, 1, f);
    fclose(f);
    riff::ChunkHeader *riffHeader = (riff::ChunkHeader *)(buf);
    riff::ChunkHeader *fmtHeader = (riff::ChunkHeader *)(buf + 12);
    wav::FormatHeader *hdr = (wav::FormatHeader *)(buf+12+8); // skip RIFF + WAV
    riff::ChunkHeader *dta = (riff::ChunkHeader *)(buf+12+8 + sizeof (wav::FormatHeader));
    auto *data = (float *)((uint8_t *)dta + sizeof(riff::ChunkHeader));
    printf("Channels: %d\n", hdr->channelCount);
    printf("SampleRate: %d\n", hdr->sampleRate);
    printf("BytesPerSample: %d\n", hdr->bytesPerSample);
    printf("BitDepth: %d\n", hdr->bitDepth);
    printf("Codec: %d\n", hdr->codec);
    if (hdr->codec != 3 || hdr->bitDepth != 32|| hdr->channelCount != 2) {
        fprintf(stderr,"ERROR Want Codec=3, BitDepth=32, Channels=2 (float32 type of samples)\n");
    }
    int nSamples = ((float*)(buf + size)-data)/2;
    printf("NSamples: %d\n", nSamples);

    try {
        for(int q=0; q<2; q++) {
            auto ctm = currentTimeMillis();
//            spdlog::info("=================================");
            ft8::decodeFT8(hdr->sampleRate, (dsp::stereo_t*)data, nSamples, [](int mode, QStringList result) {
            });
            std::cout << "Time taken: " << currentTimeMillis() - ctm << " ms" << std::endl;
        }
    } catch (std::runtime_error &e) {
        fprintf(stderr,"ERROR %s \n", e.what());
    }

}

int main(int argc, char* argv[]) {
    mshv_init();
    if (argc == 3 && !strcmp(argv[1],"--decode")) {
        doDecode(argv[2]);
        exit(0);
    }
    exit(1);
}