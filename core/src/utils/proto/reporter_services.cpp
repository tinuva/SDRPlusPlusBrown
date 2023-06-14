
#include "reporter_services.h"
#include "utils/flog.h"
#include "utils/strings.h"

namespace net {

    void getReportsFromWSPR(const std::string callsign, std::function<void(const ::net::Report &)> callback) {

    }

    void getReportsFromRBN(const std::string callsign, std::function<void(const ::net::Report &)> callback) {
        // download reports from reverse beacon network
        auto sock = net::connect("telnet.reversebeacon.net", 7000);
        uint8_t buf[10240];
        int received = sock->recv(buf, sizeof(buf), false, 10000);
        if (received < 0) {
            sock->close();
            throw std::runtime_error("Failed to receive data from RBN");
            return;
        }
        std::string data((char *) buf, received);
        if (data.find("your call") == std::string::npos) {
            sock->close();
            throw std::runtime_error("RBN: Protocol error");
        }
        int snt = sock->send((const uint8_t *) (callsign + "\r\n").c_str(), callsign.size() + 2);
        if (snt < 0) {
            sock->close();
            throw std::runtime_error("Failed to send data to RBN");
        }
        std::string ready;
        while (true) {
            auto rd = sock->recv(buf, sizeof(buf), false, net::NO_TIMEOUT);
            if (rd <= 0) {
                flog::info("EOF from RBN");
                sock->close();
                return;
            }
            buf[rd] = 0;
            ready.append((const char *) buf);
            while (true) {
                auto c = ready.find("\n");
                if (c == std::string::npos) {
                    break;
                }
                auto line = ready.substr(0, c);
                ready = ready.substr(c + 1);
                if (line.size() != 0) {
                    if (line[line.length() - 1] == '\r') {
                        line.resize(line.length() - 1);
                    }
                }
                if (line.size() != 0) {
                    if (line.find("DX de") == 0) {
                        Report report;
                        std::vector<std::string> parts;
                        splitStringV(line, " ", parts);
                        coalesceSplit(parts);
                        if (parts.size() < 12) {
                            continue;
                        }
                        report.reportingSource = RS_RBN;
                        report.reporterCallsign = parts[2];
                        auto suff = report.reporterCallsign.find("-#");
                        if (suff != std::string::npos) {
                            report.reporterCallsign = report.reporterCallsign.substr(0, suff);
                        }
                        report.reportedCallsign = parts[4];
                        if (report.reportedCallsign == callsign|| true) {
//                            report.reportedCallsign =
                        }
                        report.frequency = atof(parts[3].c_str());
                        report.mode = parts[5];
                        report.decibel = atof(parts[6].c_str());
                        report.modeParameters = parts[8] + " " + parts[9]; // 17 WPM
                        report.timestamp = parts[parts.size()-1];
                        auto tspart = line.find(report.timestamp);
                        line = line.substr(0, tspart);
                        auto wpm = line.rfind("WPM");
                        if (wpm != std::string::npos) {
                            report.extraDetail = line.substr(wpm + 4);
                            trimString(report.extraDetail);
                        }
                        callback(report);
                    }
                }
                // delete from ready
            }
        }

    }



}

void test1() {
    try {
        net::getReportsFromRBN("UR6XXX", [](auto &rep) {
            flog::info("Freq {} report {} de {}, mode {}, db {}", rep.frequency, rep.reportedCallsign, rep.reporterCallsign, rep.mode, rep.decibel);

        });
    } catch (std::exception &e) {
        flog::error("Exception: {}", e.what());
    }
}
