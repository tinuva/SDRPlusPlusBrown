#pragma once

#include <vector>
#include <utility>

struct FT8ModuleInterface {

    virtual bool isDefaultCallsign(const std::string &callsign) = 0;
    virtual std::pair<std::vector<dsp::stereo_t>, std::string> encodeCQ_FT8(const std::string &callsign, const std::string &grid, int frequency) = 0;

};

