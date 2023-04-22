#pragma once

#include "arrays.h"
#include "math.h"
#include "bgnoise.h"
#include <array>
#include <list>
#include <ctm.h>

#include "omlsa_mcra/Datablock_Read.h"

namespace dsp {
    struct omlsa_mcra {

        std::shared_ptr<Datablock_Read> dtr;
        int sampleRate = 0;
        omlsa_mcra() {
        }

        void reset() {
            dtr = std::make_shared<Datablock_Read>(sampleRate, 1, 10000);
        }

        void setSampleRate(int sampleRate) {
            this->sampleRate = sampleRate;
            reset();
        }

        int blockSize() {
            return dtr->m_wlen15;
        }

        bool process(short *in, int incount, short *out, int &outcount) {
            if (!dtr) {
                ::abort();
            }
            short abnormal_flag = dtr->Data_procese(in, out, incount, outcount);
            if (abnormal_flag < 0) return false;
            return true;
        }


    };
}
