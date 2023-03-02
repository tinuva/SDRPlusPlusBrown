#pragma once
#include <dsp/block.h>
#include <dsp/stream.h>
#include <dsp/types.h>
#include <dsp/processor.h>
#include "arrays.h"
#include "logmmse.h"

namespace dsp {

    using namespace ::dsp::arrays;
    using namespace ::dsp::logmmse;

    class IFNRLogMMSE : public Processor<complex_t, complex_t> {
        using base_type = Processor<complex_t, complex_t>;
    public:
        IFNRLogMMSE() {}

        void init(stream<complex_t>* in) override {
            base_type::init(in);
        }

        void setInput(stream<complex_t>* in) override {
            if (!_block_init) {
                init(in);
            } else {
                Processor::setInput(in);
            }
        }

    public:

        void setEffectiveSampleRate(int rate) {
            if (rate != freq) {
                freq = rate;
                params.reset();
            }
        }

        ComplexArray worker1c;
        std::mutex workerMutex;
        int freq = 192000;
        LogMMSE::SavedParamsC params;
        std::mutex freqMutex;

        void doStart() override {
            base_type::doStart();
            shouldReset = true;
        }

        void setHold(bool hold) {
            params.hold = hold;
        }

        double currentCenterFrequency = -1.0;

        bool shouldReset = false;
        void reset() {
            shouldReset = true;
        }

        int runMMSE(stream <complex_t> *_in, stream <complex_t> &out) {
            if (shouldReset) {
                flog::info("Resetting IF NR LogMMSE");
                shouldReset = false;
                worker1c.reset();
                freq = (int)sigpath::iqFrontEnd.getSampleRate();
            }
            if (!worker1c) {
                worker1c = npzeros_c(0);
                params.reset();
            }
            int count = _in->read();
            if (count < 0) { return -1; }
            for (int i = 0; i < count; i++) {
                worker1c->emplace_back(_in->readBuf[i]);
            }
            _in->flush();
            int noiseFrames = 12;
            int fram = freq / 100;
            int initialDemand = fram * 2;
            if (!params.Xk_prev) {
                initialDemand = fram * (noiseFrames + 2) * 2;
            }
            if (worker1c->size() < initialDemand) {
                return 0;
            }
            int retCount = 0;
            freqMutex.lock();
            if (!params.Xk_prev) {
                std::cout << std::endl << "Sampling initially" << std::endl;
                LogMMSE::logmmse_sample(worker1c, freq, 0.15f, &params, noiseFrames);
            }
            auto rv = LogMMSE::logmmse_all(worker1c, freq, 0.15f, &params);
            freqMutex.unlock();

            int limit = rv->size();
            auto dta = rv->data();
            for (int i = 0; i < limit; i++) {
                auto lp = dta[i];
                out.writeBuf[i] = lp * 4.0;
            }
            memmove(worker1c->data(), ((complex_t *) worker1c->data()) + rv->size(), sizeof(complex_t) * (worker1c->size() - rv->size()));
            worker1c->resize(worker1c->size() - rv->size());
            if (!out.swap(rv->size())) { return -1; }
            retCount += rv->size();
            return retCount;

        }

        int run() override {
            int count = _in->read();
            if (count < 0) {
                return -1;
            }

//            if (bypass) {
//                memcpy(out.writeBuf, _in->readBuf, count * sizeof(complex_t));
//                _in->flush();
//                if (!out.swap(count)) { return -1; }
//                return count;
//            }
//
            runMMSE(_in, out);
            return count;
        }

        void start() override {
            txHandler.ctx = this;
            txHandler.handler = [](bool txActive, void *ctx) {
                auto _this = (IFNRLogMMSE*)ctx;
                _this->params.hold = txActive;
            };
            sigpath::txState.bindHandler(&txHandler);
            block::start();
        }
        void stop() override {
            block::stop();
            sigpath::txState.unbindHandler(&txHandler);
        }

        bool bypass = true;
        EventHandler<bool> txHandler;


    };



}