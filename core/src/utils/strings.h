#pragma once

#include <functional>
#include <string>
#include <vector>
#include <cmath>
#include "memory.h"

inline void coalesceSplit(std::vector<std::string> &x) {
    for(int q=((int)x.size())-1; q>=0; q--) {
        if (x[q] == "") {
            x.erase(x.begin()+q);
        }
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

namespace percentile {

    template<typename T>
    int partition(std::vector<T>& arr, int low, int high) {
        auto pivot = arr[high];
        int i = low - 1;

        for (int j = low; j <= high - 1; ++j) {
            if (arr[j] <= pivot) {
                ++i;
                std::swap(arr[i], arr[j]);
            }
        }
        std::swap(arr[i + 1], arr[high]);
        return (i + 1);
    }

    template<typename T>
    int quick_select(std::vector<T>& arr, int low, int high, int k) {
        if (low == high) {
            return arr[low];
        }

        int pivot_index = partition<T>(arr, low, high);

        if (k == pivot_index) {
            return arr[k];
        } else if (k < pivot_index) {
            return quick_select<T>(arr, low, pivot_index - 1, k);
        } else {
            return quick_select<T>(arr, pivot_index + 1, high, k);
        }
    }

    template<typename T>
    double percentile(std::vector<T>& arr, double p) {
        int n = arr.size();
        double k = (n - 1) * p;
        int k_low = static_cast<int>(std::floor(k));
        int k_high = static_cast<int>(std::ceil(k));

        auto x_low = quick_select<T>(arr, 0, n - 1, k_low);
        auto x_high = quick_select<T>(arr, 0, n - 1, k_high);

        return x_low + (x_high - x_low) * (k - k_low);
    }


}