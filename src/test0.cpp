#include <core.h>
#include <stdio.h>
#include "../misc_modules/recorder/src/wav.h"
#include "dsp/types.h"
#include "dsp/multirate/polyphase_resampler.h"
#include "dsp/multirate/rational_resampler.h"
#include "../decoder_modules/ft8_decoder/src/ft8_decoder.h"
#include <iostream>


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
            std::cout << "================================" << std::endl;
            dsp::ft8::decodeFT8(hdr->sampleRate, (dsp::stereo_t*)data, nSamples, [](int mode, QStringList result) {
            });
        }
    } catch (std::runtime_error &e) {
        fprintf(stderr,"ERROR %s \n", e.what());
    }

}

int main(int argc, char* argv[]) {
    if (argc == 3 && !strcmp(argv[1],"--decode")) {
        doDecode(argv[2]);
        exit(0);
    }
    exit(1);
}