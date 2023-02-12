#pragma once

#include <string>
#include <functional>

std::string extractCallsignFromFT8(const std::string &message);

void splitString(const std::string & str, const char *sep, const std::function<void(const std::string&)> &callback);
void splitStringV(const std::string & str, const char *sep, std::vector<std::string> &dest);

struct LatLng {
    double lat;
    double lon;
    bool isValid() {
        return lat >= -90 && lat <= 90 && lon >= -180 && lon <= 180;
    }
    static LatLng invalid() {
        return LatLng { -1000, 0 };
    }
};

struct BearingDistance {
    double bearing;
    double distance;
};

struct CTY {

    struct Callsign {
        bool exact = false;
        LatLng ll;
        std::string continent;
        std::string value;
        std::string dxccname;

    };

    struct DXCC {

        LatLng ll;
        std::string name;
        std::string continent;  // OC,AF,EU,AS,NA,SA,AN

        std::vector<Callsign> prefixes;

    };

    std::vector<DXCC> dxcc;

    Callsign findCallsign(const std::string &callsign) const;

};

[[maybe_unused]] void loadCTY(const std::string &filename, CTY &cty);
LatLng gridToLatLng(std::string locatorString);
BearingDistance bearingDistance(LatLng fromCoords, LatLng toCoords);