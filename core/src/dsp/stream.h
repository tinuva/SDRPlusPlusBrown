#pragma once
#include <string.h>
#include <mutex>
#include <condition_variable>
#include <volk/volk.h>
#include "buffer/buffer.h"

// 1MSample buffer
#define STREAM_BUFFER_SIZE 1000000

extern void logDebugMessage(const char *msg);

namespace dsp {
    class untyped_stream {
    public:
        virtual bool swap(int size) { return false; }
        virtual int read() { return -1; }
        virtual void flush() {}
        virtual void stopWriter() {}
        virtual void clearWriteStop() {}
        virtual void stopReader() {}
        virtual void clearReadStop() {}
    };




    template <class T>
    class stream : public untyped_stream {
    public:

        const char *origin;
        const char originBuf[100] = "stream without origin";

        stream() {
            static int streamCount = 0;
            int sc = streamCount++;
            sprintf((char*)originBuf, "stream %d", sc);
            if (sc == 27 || sc == 28 || sc > 1000000) {
                switch(sc) {
                case 27:
                    logDebugMessage("stream 27");
                    break;
                case 28:
                    logDebugMessage("stream 28");
                    break;
                default:
                    abort();
                }
            }
            this->origin = &originBuf[0];
            initBuffers();
        }
        stream(const char *origin) : stream() {
            this->origin = origin;
        }

        void initBuffers() {
            writeBuf0 = buffer::alloc<T>(STREAM_BUFFER_SIZE);
            if (!writeBuf0)
                abort();
            buffer::register_buffer_dbg(writeBuf0, origin ? origin: "stream without origin");
            readBuf0 = buffer::alloc<T>(STREAM_BUFFER_SIZE);
            if (!readBuf0)
                abort();
            buffer::register_buffer_dbg(readBuf0, origin ? origin: "stream without origin");
            readBuf = readBuf0;
            writeBuf = writeBuf0;
        }

        virtual ~stream() {
            free();
        }

        virtual void setBufferSize(int samples) {
            if (!writeBuf) {
                abort();
            }
            buffer::free(writeBuf0);
            buffer::free(readBuf0);
            writeBuf0 = buffer::alloc<T>(samples);
            readBuf0 = buffer::alloc<T>(samples);
            buffer::register_buffer_dbg(writeBuf0, origin ? origin: "stream without origin, sbs");
            buffer::register_buffer_dbg(readBuf0, origin ? origin: "stream without origin, sbs");
            readBuf = readBuf0;
            writeBuf = writeBuf0;

        }

        virtual inline bool swap(int size) {
            {
                // Wait to either swap or stop
                std::unique_lock<std::mutex> lck(swapMtx);
                swapCV.wait(lck, [this] { return (canSwap || writerStop); });

                // If writer was stopped, abandon operation
                if (writerStop) { return false; }

                // Swap buffers
                dataSize = size;
                T* temp = writeBuf;
                writeBuf = readBuf;
                readBuf = temp;
                canSwap = false;
            }

            // Notify reader that some data is ready
            {
                std::lock_guard<std::mutex> lck(rdyMtx);
                dataReady = true;
            }
            rdyCV.notify_all();

            return true;
        }

        virtual inline int read() {
            // Wait for data to be ready or to be stopped
            if (this->origin == "merger.out") {
                flog::info("stream::read:: called on from merger.out..");
            }
            std::unique_lock<std::mutex> lck(rdyMtx);
            rdyCV.wait(lck, [this] { return (dataReady || readerStop); });


            auto rv = readerStop ? -1 : dataSize;
            if (this->origin == "merger.out") {
                flog::info("stream::read:: has been read from merger.out: {}", rv);
            }
            return (rv);
        }

        virtual inline bool isDataReady() {
            {
                std::lock_guard<std::mutex> lck(rdyMtx);
                return dataReady;
            }
        }

        virtual inline void flush() {
            // Clear data ready
            {
                std::lock_guard<std::mutex> lck(rdyMtx);
                dataReady = false;
            }

            // Notify writer that buffers can be swapped
            {
                std::lock_guard<std::mutex> lck(swapMtx);
                canSwap = true;
            }

            swapCV.notify_all();
        }

        virtual void stopWriter() {
            {
                std::lock_guard<std::mutex> lck(swapMtx);
                writerStop = true;
            }
            swapCV.notify_all();
        }

        virtual void clearWriteStop() {
            writerStop = false;
        }

        virtual void stopReader() {
            {
                std::lock_guard<std::mutex> lck(rdyMtx);
                readerStop = true;
            }
            rdyCV.notify_all();
        }

        virtual void clearReadStop() {
            readerStop = false;
        }

        void free() {
            if (writeBuf0) { buffer::free(writeBuf0); }
            if (readBuf0) { buffer::free(readBuf0); }
            writeBuf0 = NULL;
            readBuf0 = NULL;
            writeBuf = NULL;
            readBuf = NULL;
        }

        T* writeBuf;
        T* readBuf;
        T* writeBuf0;
        T* readBuf0;

    private:

        int initialized = 0;

        std::mutex swapMtx;
        std::condition_variable swapCV;
        bool canSwap = true;

        std::mutex rdyMtx;
        std::condition_variable rdyCV;
        bool dataReady = false;

        bool readerStop = false;
        bool writerStop = false;

        int dataSize = 0;
    };
}