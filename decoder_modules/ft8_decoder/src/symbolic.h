#pragma once

#include <string>
#include <functional>
#include <vector>

std::string extractCallsignFromFT8(const std::string &message);

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
    BearingDistance() : bearing(0), distance(0) {}
    BearingDistance(double bearing, double distance) : bearing(bearing), distance(distance) {}
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

[[maybe_unused]] void loadCTY(const std::string &filename, const std::string &region, CTY &cty);
LatLng gridToLatLng(std::string locatorString);
BearingDistance bearingDistance(LatLng fromCoords, LatLng toCoords);