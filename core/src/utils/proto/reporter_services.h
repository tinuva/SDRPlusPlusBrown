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
        std::string reporterCallsign;
        std::string reportedCallsign;
        std::string timestamp;
        std::string mode;
        std::string modeParameters;
        std::string extraDetail;
        float decibel;
        float frequency; // in Hz

    };

}