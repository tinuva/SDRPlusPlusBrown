#include <core.h>
#include <iostream>
#include <stdio.h>
#include <module.h>
#include <ctm.h>
#include <utils/wav.h>
#include <utils/riff.h>
#include "dsp/types.h"
#include "dsp/multirate/polyphase_resampler.h"
#include "dsp/multirate/rational_resampler.h"

#include "ft8_etc/mshv_support.h"
#include "ft8_etc/mscore.h"
#include "ft8_etc/decoderms.h"

extern void doDecode(const char *path, std::function<void(int mode, std::vector<std::string> result)> callback);

int main(int argc, char* argv[]) {
    if (argc >= 3 && !strcmp(argv[1],"--decode")) {
        doDecode(argv[2], [](int mode, std::vector<std::string> result) {
        });
        exit(0);
    }
    exit(1);
}