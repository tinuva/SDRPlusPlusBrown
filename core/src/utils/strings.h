#pragma once

#include <functional>
#include <string>
#include <vector>
#include "memory.h"

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



inline void splitStringV(const std::string & str, const char *sep, std::vector<std::string> &dest) {
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

