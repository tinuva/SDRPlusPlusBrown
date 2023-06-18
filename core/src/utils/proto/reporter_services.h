#pragma once

#include <string>
#include <functional>
#include "http.h"
#include <utils/flog.h>

namespace net {

    enum ReportingSource {
        RS_RBN = 1,
        RS_WSPRNET = 2,
        RS_PSKREPORTER = 3
    };

    struct Report {
        ReportingSource reportingSource;
        // either
        std::string errorStatus;
        //
        std::string reporterCallsign;
        std::string reportedCallsign;
        std::string timestamp;
        std::string mode;
        std::string modeParameters;
        std::string receiverLocator;
        float decibel;
        float frequency; // in Hz
        long long createdTimestamp;
    };

    void getReportsFromRBN(const std::string telnetCallsign, const std::string callsign, std::function<void(const ::net::Report &)> callback, bool &running);
    void getReportsFromPSKR(const std::string callsign, std::function<void(const ::net::Report &)> callback, bool &running);
    bool getReportsFromWSPR(const std::string callsign, std::function<void(const ::net::Report &)> callback, bool &running);

}