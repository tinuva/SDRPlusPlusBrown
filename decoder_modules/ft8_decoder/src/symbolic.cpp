
#define _USE_MATH_DEFINES
#include <cmath>

#include <fstream>
#include <cstring>
#include "symbolic.h"

static bool isValidLocatorString(const std::string &locatorString) {
    return tolower(locatorString[0]) >= 'a' && tolower(locatorString[0]) <= 'r' && tolower(locatorString[1]) >= 'a' && tolower(locatorString[1]) <= 'r'
           && locatorString[2] >= '0' && locatorString[2] <= '9' && locatorString[3] >= '0' && locatorString[3] <= '9'
           && (locatorString.length() == 4 || (locatorString.length() == 6 && locatorString[4] >= 'a' && locatorString[4] <= 'x' && locatorString[5] >= 'a' && locatorString[5] <= 'x'));
}

static const int CHAR_CODE_OFFSET = 65;


static int charToNumber(char c) {
    return std::toupper(c) - CHAR_CODE_OFFSET;
}



LatLng gridToLatLng(std::string locatorString) {
    if (locatorString.length() == 4) {
        locatorString = locatorString + "ll";
    }
    if (!isValidLocatorString(locatorString)) {
        return LatLng::invalid();
    }

    auto fieldLng = charToNumber(locatorString[0]) * 20;
    auto fieldLat = charToNumber(locatorString[1]) * 10;
    auto squareLng = std::stod(std::string()+locatorString[2]) * 2;
    auto squareLat = std::stod(std::string()+locatorString[3]);
    auto subsquareLng = (charToNumber(locatorString[4]) + 0.5) / 12;
    auto subsquareLat = (charToNumber(locatorString[5]) + 0.5) / 24;

    LatLng ll;
    ll.lat = fieldLat + squareLat + subsquareLat - 90;
    ll.lon = fieldLng + squareLng + subsquareLng - 180;
    return ll;
}

static double degToRad(double d) {
    while(d < 0) {
        d += 360;
    }
    while(d >= 360) {
        d -= 360;
    }
    return d * M_PI / 180;
}

static double radToDeg(double rad) {
    double d = rad / M_PI * 180;
    while(d < 0) {
        d += 360;
    }
    while(d >= 360) {
        d -= 360;
    }
    return d;
}

BearingDistance bearingDistance(LatLng fromCoords, LatLng toCoords) {
    // fi = lat
    // lam = lon
    auto dLat = degToRad(toCoords.lat - fromCoords.lat); // dfi
    auto dLon = degToRad(toCoords.lon - fromCoords.lon); // dlam
    auto fromLat = degToRad(fromCoords.lat); // fi 1
    auto toLat = degToRad(toCoords.lat); // fi 2
    auto a =
        pow(sin(dLat / 2), 2) + pow(sin(dLon / 2), 2) * cos(fromLat) * cos(toLat);
    auto b = 2 * atan2(sqrt(a), sqrt(1 - a));

    auto y = sin(dLon) * cos(toLat);
    auto x = cos(fromLat) * sin(toLat) - sin(fromLat) * cos(toLat) * cos(dLon);

    auto az = atan2(y, x);

    if (az < 0) {
        az += 2 * M_PI;
    }

    return BearingDistance( az, b * 6371);
}


bool isSignalStrength(const std::string &maybeStrength) {
    if (maybeStrength.length() < 2) return false;
    if (maybeStrength[0] != '+' && maybeStrength[0] != '-') return false;
    for (int i = 1; i < maybeStrength.length(); i++) {
        if (!std::isdigit(maybeStrength[i])) return false;
    }
    return true;
}

bool isLocator(const std::string &maybeLoc) {
    if (maybeLoc.length() != 4) return false;
    if (std::isupper(maybeLoc[0]) && std::isupper(maybeLoc[1]) && std::isalpha(maybeLoc[0]) && std::isalpha(maybeLoc[1]) && std::isdigit(maybeLoc[2]) && std::isdigit(maybeLoc[3])) {
        return true;
    }
    return false;
}

std::string extractCallsignFromFT8(const std::string &message) {
    std::vector<std::string> parts;
    splitString(message," ",[&](const std::string &str) {
        if (!str.empty()) {
            parts.emplace_back(str);
        }
    });
    if (parts.empty()) {
        return "";
    }
    auto &lastPart = parts.back();
    if (parts.size() > 1 && (lastPart == "RR73" || lastPart == "RRR" || lastPart == "73" || isLocator(lastPart))) {       /// .... CALL KO80
        return parts[parts.size()-2];
    }
    if (lastPart.size() > 2 && (lastPart[0] == 'R' && isSignalStrength(lastPart.substr(1)) || isSignalStrength(lastPart))) {        // DXCALL CALL R+10
        return parts[parts.size()-2];
    }
    if (parts.size() == 2 && parts[0] == "CQ") {    // cq without loc
        return parts[1];
    }

    return "";
}

void splitStringV(const std::string & str, const char *sep, std::vector<std::string> &dest) {
    dest.clear();
    splitString(str,sep,[&](const std::string &str) {
        dest.emplace_back(str);
    });
}


inline void trimString(std::string & line) {
    while (!line.empty() && std::isspace(line.back())) {
        line.pop_back();
    }
    while (!line.empty() && std::isspace(line.front())) {
        line.erase(line.begin());
    }
}

inline void splitString(const std::string & str, const char *sep, const std::function<void(const std::string&)> &callback) {
    const char *c = str.data();
    size_t limit = str.length();
    if (limit == 0) {
        return;
    }
    long long start = 0;
    for (long long i = 0; i < limit; i++) {
        if (i == limit - 1) {
            if (strchr(sep, c[i]) != nullptr) {
                callback(std::string(c + start, i - start));
            } else {
                callback(std::string(c + start, i - start + 1));
            }
            break;
        }
        if (strchr(sep, c[i]) != nullptr) {
            callback(std::string(c + start, i - start));
            start = i + 1;
        }
    }
}

static std::string getText(const std::string &txt, int &i, char end) {
    std::string part;
    i++;
    for(i++; i<txt.length(); i++) {
        if (txt[i] == end) {
            break;
        }
        part += txt[i];
    }
    return part;
}


static void parseCallsign(const std::string &txt, CTY::Callsign* pCallsign) {
    std::string part;
    std::vector<std::string> parts;
    int i=0;
    if (txt.empty()) {
        return;
    }
    if (txt[i] == '=') {
        i++;
        pCallsign->exact = true;
    } else {
    }
    bool endValue = false;
    for(; i<txt.length(); i++) {
        switch(txt[i]) {
        case '{':
            endValue = true;
            part = getText(txt, i, '}');
            pCallsign->continent = part;
            break;
        case '<':
            part = getText(txt, i, '>');
            splitStringV(part, "/", parts);
            if (parts.size() == 2) {
                pCallsign->ll.lat = std::stod(parts[0]);
                pCallsign->ll.lon = -std::stod(parts[1]);
            }
            endValue = true;
            continue;
        case '[': // ITU
            getText(txt, i, ']');
            endValue = true;
            continue;
        case '(': // CQ
            getText(txt, i, ')');
            endValue = true;
            continue;
        }
        if (!endValue) {
            pCallsign->value += txt[i];
        }
    }
}

static bool isSomeWeirdName(const std::string &basicString) {
    int countDashes = 0;
    for(auto c: basicString) {
        if (c == '-') {
            countDashes++;
        }
    }
    return countDashes > 1;
}


void loadCTY(const std::string& filename, const std::string &region, CTY& cty) {
    bool excludeWeirdThings = !region.empty();
    std::ifstream file(filename);
    if (!file.is_open()) {
        throw std::runtime_error("Unable to open CTY file: "+filename);
    }
    std::string line;
    std::vector<std::string> lineSplit;
    bool isWeirdSection = false;
    while (std::getline(file, line)) {
        if (line.empty()) continue;
        if (line[0] != ' ') {
            splitStringV(line, ":", lineSplit);
            for(auto &s: lineSplit) {
                trimString(s);
            }
            if (lineSplit.size() >= 8) {
                auto& locName = lineSplit[0];
                isWeirdSection = isSomeWeirdName(locName);
                if (!excludeWeirdThings || !isWeirdSection) {
                    cty.dxcc.emplace_back(CTY::DXCC{ std::stod(lineSplit[4]), -std::stod(lineSplit[5]), locName + region, lineSplit[3] });
                }
            }
            continue;
        }
        if (!excludeWeirdThings || !isWeirdSection) {
            splitStringV(line, " ,;\r\n", lineSplit);
            for (auto s : lineSplit) {
                trimString(s);
                if (!cty.dxcc.empty()) {
                    CTY::Callsign cs;
                    parseCallsign(s, &cs);
                    if (cs.value.empty()) {
                        continue;
                    }
                    cty.dxcc.back().prefixes.emplace_back(cs);
                }
            }
        }
    }
    file.close();
}


CTY::Callsign CTY::findCallsign(const std::string& callsign) const {
    bool found = false;
    CTY::Callsign rv;
    for(auto & dxcc1 : dxcc) {
        for(auto &prefix: dxcc1.prefixes) {
            if (prefix.exact) {
                if (callsign == prefix.value) {
                    rv = prefix;
                    rv.ll = dxcc1.ll;
                    rv.continent = dxcc1.continent;
                    rv.dxccname = dxcc1.name;
                    found = true;
                }
            }
        }
    }
    if (found) { // exact
        return rv;
    }
    for(auto & dxcc1 : dxcc) {
        for(auto &prefix: dxcc1.prefixes) {
            if (!prefix.exact) {
                if (callsign.find(prefix.value) == 0)  {    // last one is more important
                    if (prefix.value.length() >= rv.value.length()) {
                        rv = prefix;
                        rv.ll = dxcc1.ll;
                        rv.continent = dxcc1.continent;
                    }
                    if (rv.dxccname.empty() || !isSomeWeirdName(dxcc1.name)) {
                        rv.dxccname = dxcc1.name;
                    }
                }
            }
        }
    }
    return rv;
}
