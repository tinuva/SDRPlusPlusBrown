#pragma once
#include "../sink.h"
#include <atomic>


/**
 * this one copies data from any input to output, using the secondary input as higher priority stream.
 * if any secondary streams have no data, default input is used.
 */

namespace dsp::routing {
    template <class T>
    class Merger : public Sink<T> {
        using base_type = Sink<T>;
    public:
        Merger() {}

        Merger(stream<T>* out) {
            base_type::init(nullptr);
            this->registerOutput(out);
        }

        void bindStream(int priority, stream<T>* stream) {
            assert(base_type::_block_init);
            std::lock_guard<std::recursive_mutex> lck(base_type::ctrlMtx);

            for(auto &s : secondaryStreams) {
                if (s.second == stream) {
                    throw std::runtime_error("[Splitter] Tried to bind stream to that is already bound");
                }
            }
            // Add to the list
            auto newStream = std::make_shared<SecondaryStream>();
            newStream->priority = priority;
            newStream->astream = stream;
            secondaryStreams.push_back(newStream);
            this->registerInput(stream);

            std::thread x([&]() {
                while(true) {
                    int rd = stream->read();
                    if (rd < 0) {
                        break;
                    }
                    newStream->dataLock.lock();
                    std::copy(stream->readBuf, stream->readBuf + rd, newStream->data.end());
                    newStream->hasData.store(true);
                    newStream->dataLock.unlock();
                    dataReadyMutex.lock();
                    dataReady.notify_one();
                    dataReadyMutex.unlock();
                }
            });
        }

        void unbindStream(stream<T>* stream) {
            assert(base_type::_block_init);
            std::lock_guard<std::recursive_mutex> lck(base_type::ctrlMtx);
            
            for(auto &s : secondaryStreams) {
                if (s.second == stream) {
                    secondaryStreams.erase(s);
                    s.second->stopReader();
                    return;
                }
            }
            this->unregisterInput(stream);
        }

        int run() {
            {
                std::unique_lock<std::mutex> lk(dataReadyMutex);
                dataReady.wait(lk);
            }
            std::lock_guard<std::recursive_mutex> lck(base_type::ctrlMtx);
            SecondaryStream *bestStream = nullptr;
            for(auto &ss : secondaryStreams) {
                if (ss->hasData.load()) {
                    if (!bestStream || ss.priority < bestStream->priority) {
                        bestStream = ss;
                    }
                }
            }
            for(auto &ss : secondaryStreams) {
                if (bestStream && ss != bestStream) {
                    ss->dataLock.lock();
                    ss->data.clear();
                    ss->dataLock.unlock();
                }
            }
            if (bestStream) {
                bestStream->dataLock.lock();
                int count = bestStream->data.size();
                memcpy(base_type::_out->writeBuf, bestStream->data.data(), count * sizeof(T));
                bestStream->data.clear();
                bestStream->dataLock.unlock();
            }
            return 1;
        }


        struct SecondaryStream {
            int priority;
            stream<T>* astream;
            std::vector<T> receivedData;
            std::mutex dataLock;
            std::atomic<bool> hasData;
        };

        std::vector<std::shared_ptr<SecondaryStream>> secondaryStreams;

        std::condition_variable dataReady;
        std::mutex dataReadyMutex;


    };
}