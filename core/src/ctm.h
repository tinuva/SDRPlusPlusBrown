#pragma once

#include <chrono>

inline long long currentTimeMillis() {
    std::chrono::system_clock::time_point t1 = std::chrono::system_clock::now();
    long long msec = std::chrono::time_point_cast<std::chrono::milliseconds>(t1).time_since_epoch().count();
    return msec;

}