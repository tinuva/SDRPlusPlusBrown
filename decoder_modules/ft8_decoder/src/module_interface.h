#pragma once

struct FT8ModuleInterface {

    virtual bool isDefaultCallsign(const std::string &callsign) = 0;
    virtual std::vector<dsp::stereo_t> encodeCQ(const std::string &callsign, const std::string &grid) = 0;

};

