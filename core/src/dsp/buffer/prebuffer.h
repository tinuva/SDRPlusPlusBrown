#pragma once

#include "../types.h"
#include "../processor.h"
#include "../utils/usleep.h"
#include "../ctm.h"


namespace dsp::buffer {

    template<typename T>
    class Prebuffer : public Processor<T, T> {
        using base_type = Processor<T, T>;
    public:
        Prebuffer() {}

        Prebuffer(stream<T> *in) { base_type::init(in); }

        int prebufferMsec = 0;
        std::mutex bufferLock;
        std::vector<T> buffer;
        bool bufferReached = false;
        void setPrebufferMsec(int msec) {
            this->prebufferMsec = msec;
        }

        int sampleRate = 48000;
        void setSampleRate(int sampleRate) {
            this->sampleRate = sampleRate;
            clear();
        }

        bool volatile running = false;

        int getBufferSize() const {
            return prebufferMsec * sampleRate / 1000;
        }

        virtual void doStart() {
            base_type ::doStart();
            running = true;
            std::thread([=]() {
                flog::info("Prebuffer: begin: running={}, &running={}",running, (void*)&running);
                const int ITER_STEP_MILLIS = 50;
                long long lastSend;
                while (running) {
                    // this is long loop wating until preload goal is reached
                    // preload goal may be set again if underflow during normal op.
                    while(running && !bufferReached) {
                        bufferLock.lock();
                        auto bs = buffer.size();
                        bufferLock.unlock();
                        bufferReached = bs >= getBufferSize();
                        if (bufferReached) {
                            lastSend = currentTimeMillis() - ITER_STEP_MILLIS;
                            break;
                        }
                        usleep(ITER_STEP_MILLIS);
                    }
                    if (!running) {
                        break;
                    }
                    // buffer has been reached. Check if enough data.
                    auto ctm = currentTimeMillis();
                    long long samplesNeeded = (ctm - lastSend) * sampleRate / 1000;
                    lastSend = ctm;
                    if (running) {
                        bufferLock.lock();
                        auto bs = buffer.size();
                        bufferLock.unlock();
                        if (bs >= samplesNeeded) {
                            // fall through
                        } else {
                            // not enough data in buffer to send.
                            bufferReached = false; // want to fill again.
                            usleep(ITER_STEP_MILLIS);
                            continue; // sleep again.
                        }
                    }

                    if (samplesNeeded == 0) {
                        usleep(ITER_STEP_MILLIS);
                        continue; // sleep again.
                    }

                    bufferLock.lock();
                    std::copy(buffer.begin(), buffer.begin() + samplesNeeded, base_type::out.writeBuf);
                    buffer.erase(buffer.begin(), buffer.begin() + samplesNeeded);
                    if (getBufferSize() > 0) {
                        if (buffer.size() > getBufferSize()) {
                            // drop over 100%
                            auto drop = buffer.size() - getBufferSize();
                            buffer.erase(buffer.begin(), buffer.begin() + drop);
                        }
                    }
                    bufferLock.unlock();
                    base_type::out.swap(samplesNeeded);
                }
                flog::info("Prebuffer: finished, running={}, &running={}",running, (void*)&running);
                return ;
            }).detach();
        }

        virtual void doStop() {
            base_type::doStop();
            running = false;
        }

        void clear() {
            bufferLock.lock();
            buffer.clear();
            bufferReached = false;
            bufferLock.unlock();
        }

        long long lastReceived = 0;

        int getTimeDelayInMillis() { // buffer head time delay from last arrived packet.
            if (sampleRate == 0) {
                return 0;
            }
            return buffer.size() * 1000 / sampleRate;
        }

        int getPercentFull() {
            bufferLock.lock();
            if (getBufferSize() <= 0) {
                bufferLock.unlock();
                return 100;
            }
            int percent = 100 * buffer.size() / getBufferSize();
            bufferLock.unlock();
            return percent;
        }

        int run() {
            int count = base_type::_in->read();
            if (count < 0) { return -1; }
            lastReceived = currentTimeMillis();
            bufferLock.lock();
            buffer.insert(buffer.end(), base_type::_in->readBuf, base_type::_in->readBuf + count);
            bufferLock.unlock();
            base_type::_in->flush();
            return 0;
        }

    };

}