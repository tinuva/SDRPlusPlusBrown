#pragma once

#include <string>
#include <functional>

std::string extractCallsignFromFT8(const std::string &message);

void splitString(const std::string & str, char sep, const std::function<void(const std::string&)> &callback);

