#pragma once
#include <json.hpp>
#include <imgui/imgui.h>
#include <stdint.h>

using nlohmann::json;

namespace geomap {

    struct GeoMap {

        void draw();
    };

};