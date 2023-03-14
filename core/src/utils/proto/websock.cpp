#include "websock.h"
#include <ctm.h>
#include <thread>
#include <utils/flog.h>


void test1() {
    try {
        net::websock::WSClient x;
        // ws://220.235.76.11:8073/kiwi/1678569640729/SND
        auto ctm = currentTimeMillis();
        x.onConnected = [&]() {
            //x.sendString("SET mod=usb low_cut=300 high_cut=2700 freq=14100.000");
            x.sendString("SET auth t=kiwi p=#");
            x.sendString("SET AR OK in=12000 out=48000");
            x.sendString("SET mod=iq low_cut=-5000 high_cut=5000 freq=9604.330");
//            x.sendString("SET mod=am low_cut=-4900 high_cut=4900 freq=119604.33");
            x.sendString("SERVER DE CLIENT sdr++brown SND");
            x.sendString("SET compression=0");
            x.sendString("SET agc=1 hang=0 thresh=-100 slope=6 decay=1000 manGain=50");

        };
        x.onTextMessage = [&](std::string msg) {
            flog::info("TEXT: {}", msg);
        };
        x.onBinaryMessage = [&](std::string msg) {
            std::string start = "???";
            if (msg.size() >= 3) {
                start = msg.substr(0, 3);
            }
            if (start == "MSG") {
                flog::info("BIN/MSG: {} text: {}", (int64_t)msg.size(), msg);
            } else {
                if (msg.size() >= 70) {
                    char buf[100];
                    for (int q = 3; q < 30; q++) {
                        sprintf(buf, "%02x ", (unsigned char)msg[q]);
                        start += buf;
                    }
                    start += "... ";
                    for (int q = -20; q < 0; q++) {
                        sprintf(buf, "%02x ", (unsigned char)msg[msg.size()-1+q]);
                        start += buf;
                    }
                }
                flog::info("BIN: {} bytes: {}", (int64_t)msg.size(), start);
            }
        };
        auto lastPing = currentTimeMillis();
        x.onEveryReceive = [&]() {
            if (currentTimeMillis() - lastPing > 4000) {
                x.sendString("SET keepalive");
                lastPing = currentTimeMillis();
            }
        };
        x.connectAndReceiveLoop("jp1odj-air.proxy.kiwisdr.com", 8073, "/kiwi/" + std::to_string(ctm) + "/SND");
    } catch (std::runtime_error& e) {
        flog::error("test1: {}", e.what());
    }
}

