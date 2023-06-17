
#include "reporter_services.h"
#include "utils/flog.h"
#include "utils/strings.h"
#include "mqtt.h"
#include "json.hpp"
#include "utils/usleep.h"

namespace net {

    void getReportsFromWSPR(std::string callsign, std::function<void(const ::net::Report &)> callback, bool &running) {
        auto reportError = [&](std::string error) {
            ::net::Report report;
            report.reportingSource = ::net::RS_WSPRNET;
            report.errorStatus = error;
            callback(report);
            usleep(30000000);
        };
        while(running) {
            std::string formBuildId;
            try {
                Report rep;
                rep.reportingSource = RS_WSPRNET;
                rep.errorStatus = "Checking..";
                callback(rep);
                auto emptyForm = net::http::get("http://www.wsprnet.org:80/drupal/wsprnet/spotquery");
                std::vector<std::string> lines;
                splitStringV(emptyForm, "\n", lines);
                // find string in lines, with form_id=wsprnet_spotquery_form
                for(int q=0; q<lines.size(); q++) {
                    if (lines[q].find("value=\"wsprnet_spotquery_form\"") != std::string::npos) {
                        // previous one contains the form_build_id
                        int index = lines[q - 1].find("value=\"form-");
                        if (index != std::string::npos) {
                            auto here = lines[q - 1].substr(index  + 7);
                            int index2 = here.find("\"");
                            if (index2 != std::string::npos) {
                                formBuildId = here.substr(0, index2);
                                break;
                            }

                        }
                    }

                }
                if (formBuildId == "") {
                    reportError("wsprnet http form not found");
                    continue;
                }
                auto formData = "band=All&mode=All&count=100&timelimit=3600&sortby=date&call=" + callsign + "&reporter=&sortrev=1&excludespecial=1&op=update&form_id=wsprnet_spotquery_form&form_build_id="+formBuildId;
                auto searchResults = net::http::post("http://www.wsprnet.org:80/drupal/wsprnet/spotquery", formData);
                flog::info("post result:\n{}", searchResults);
                auto spots = searchResults.find("spots:</p>");
                if (spots == std::string::npos) {
                    reportError("unparseable response.");
                    continue;
                }
                searchResults = searchResults.substr(spots + 9);
                splitStringV(searchResults,"\n", lines);
                std::vector<std::string> fields;
                for(auto q=0; q<lines.size(); q++) {
                    auto line = lines[q];
                    if (line.find("<tr><td align='right'>&nbsp;20") == 0) {  // 20XX is year => this is a valid response.
                        removeSubstrings(line, "align='right'>");
                        removeSubstrings(line, "align='left'>");
                        removeSubstrings(line, "<tr>");
                        removeSubstrings(line, "</tr>");
                        removeSubstrings(line, "</td>");
                        removeSubstrings(line, "&nbsp;");
                        replaceSubstrings(line, "<td ","\t");
                        splitStringV(line, "\t", fields);
                        if (fields.size() >= 13) {
                            fields.erase(fields.begin());
                        }
                        flog::info("{}", line);
                        Report report;
                        report.timestamp = fields[0].substr(11)+"00Z";
                        report.reporterCallsign = fields[7];
                        report.reportedCallsign = fields[1];
                        report.frequency = atof(fields[2].c_str())*1000000;
                        report.reportingSource = RS_WSPRNET;
                        report.decibel = atof(fields[3].c_str());
                        report.mode = fields[11];
                        report.modeParameters = fields[4];  // drift
                        report.receiverLocator = fields[8]; // receiver loc
                        callback(report);
                    }
                }
                usleep(15000000);
                rep.reportingSource = RS_WSPRNET;
                rep.errorStatus = "Checked, sleeping.";
                callback(rep);
            } catch (std::exception &e) {
                reportError(e.what());
                continue;
            }
        }

    }

    void getReportsFromPSKR(std::string callsign, std::function<void(const ::net::Report &)> callback, bool &running) {
        auto reportError = [&](std::string error) {
            ::net::Report report;
            report.reportingSource = ::net::RS_PSKREPORTER;
            report.errorStatus = error;
            callback(report);
            usleep(30000000);
        };
        while(running) {
            MQTTClient client;
            if (!client.socket_connect("mqtt.pskreporter.info", 1883)) {
                reportError("Cannot connect to pskreporter");
                continue;
            }
            client.receive_callback = [&](const char *topic, const uint8_t *payload, uint16_t len) {
                char buf[1024];
                memcpy(buf, payload, len);
                buf[len] = 0;
                try {
                    Report rep;
                    auto obj = nlohmann::json::parse(buf);
                    time_t t = obj["t"];
                    rep.reportedCallsign = obj["sc"];
                    rep.reporterCallsign = obj["rc"];
                    rep.decibel = obj["rp"];
                    rep.mode = obj["md"];
                    rep.receiverLocator = obj["rl"];
                    rep.frequency = obj["f"];
                    auto tmm = std::gmtime(&t);
                    char streamTime[64];
                    strftime(streamTime, sizeof(streamTime), "%H:%M:%SZ", tmm);
                    rep.timestamp = streamTime;
                    callback(rep);
                } catch (std::exception &e) {
                    return;
                }
            };
            if (!client.subscribe("pskr/filter/v2/+/FT8/" + callsign + "/+/+/+/+/+")) {
                client.stop();
                reportError("Could not send subscribe message to pskreporter, will retry");
                continue;
            }
            Report rep;
            rep.reportingSource = RS_PSKREPORTER;
            rep.errorStatus = "MQTT connected.";
            callback(rep);
            while (running) {
                if (!client.update()) {
                    break;
                }
            }
            reportError("Socket closed, will retry");
        }

    }

    void getReportsFromRBN(const std::string callsign, std::function<void(const ::net::Report &)> callback, bool &running) {
        while(running) {
            // download reports from reverse beacon network
            auto sock = net::connect("telnet.reversebeacon.net", 7000);
            uint8_t buf[10240];
            int received = sock->recv(buf, sizeof(buf), false, 10000);
            if (received < 0) {
                sock->close();
                throw std::runtime_error("Failed to receive data from RBN");
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
            while (running) {
                auto rd = sock->recv(buf, sizeof(buf), false, 10000);
                if (rd == 0) { // timeout?
                    if (!sock->isOpen()) {
                        break;
                    }
                    continue;
                }
                if (rd <= 0) {
                    flog::info("EOF from RBN");
                    sock->close();
                    return;
                }
                Report rep;
                rep.reportingSource = RS_WSPRNET;
                rep.errorStatus = "telnet connected.";
                callback(rep);
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
                            if (report.reportedCallsign == callsign || true) {
                                //                            report.reportedCallsign =
                            }
                            report.frequency = atof(parts[3].c_str());
                            report.mode = parts[5];
                            report.decibel = atof(parts[6].c_str());
                            report.modeParameters = parts[8] + " " + parts[9]; // 17 WPM
                            report.timestamp = parts[parts.size() - 1]; // 1530z
                            auto tspart = line.find(report.timestamp);
                            line = line.substr(0, tspart);
                            report.timestamp = report.timestamp.substr(0, 2) + ":" + report.timestamp.substr(2, 2) + ":00Z";
                            auto wpm = line.rfind("WPM");
                            if (wpm != std::string::npos) {
                                auto txt = line.substr(wpm + 4);
                                trimString(txt);
                                report.modeParameters = report.modeParameters + ": " + txt;
                            }
                            if (report.reportedCallsign.find(callsign) != std::string::npos) {
                                callback(report);
                            }
                        }
                    }
                    // delete from ready
                }
            }
        }

    }



}

void test1() {
    try {
        bool running = true;
        net::getReportsFromRBN("9A9RA", [](auto &rep) {
            flog::info("Freq {} report {} de {}, mode {}, db {}", rep.frequency, rep.reportedCallsign, rep.reporterCallsign, rep.mode, rep.decibel);
        }, running);
    } catch (std::exception &e) {
        flog::error("Exception: {}", e.what());
    }
    abort();
}
