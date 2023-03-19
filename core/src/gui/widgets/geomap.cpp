
#define IMGUI_DEFINE_MATH_OPERATORS
#include <gui/widgets/geomap.h>
#include <fstream>
#include <core.h>
#include <utils/wstr.h>
#include <utils/flog.h>
#include <filesystem>
#include <sstream>
#include <iomanip>
#include <random>
#include <imgui/imgui_internal.h>

namespace geomap {

    constexpr double pi = 3.14159265358979323846;
    constexpr double earthRadius = 6371.0; // Earth radius in kilometers

    // Converts degrees to radians
    inline double degToRad(double deg) {
        return deg * pi / 180.0;
    }

    struct GeoCoordinates {
        double latitude;
        double longitude;
    };

    struct CartesianCoordinates {
        double x;
        double y;
    };


    CartesianCoordinates geoToCartesian(const GeoCoordinates& geo) {
        double latRad = degToRad(geo.latitude);
        double lngRad = degToRad(geo.longitude);

        double x = lngRad / pi;
        double y = latRad / (pi / 2.0);

        return { x, y };
    }

    nlohmann::json geoJSON;
    std::vector<std::vector<std::vector<CartesianCoordinates>>> countriesGeo;

    nlohmann::json readGeoJSONFile(const std::string& filePath) {
        std::ifstream fileStream(filePath);

        if (!fileStream.is_open()) {
            flog::error("Failed to open the file {}", filePath);
            return nullptr;
        }

        nlohmann::json geoJSON;
        fileStream >> geoJSON;

        return geoJSON;
    }

    void maybeInit() {
        if (geoJSON.empty()) {
            std::string resDir = core::configManager.conf["resourcesDirectory"];
            const std::string filePath = resDir + "/cty/map.json";
            geoJSON = readGeoJSONFile(filePath);


            for (const auto& feature : geoJSON["features"]) {
                std::string countryID = feature["properties"]["NAME"];
                countriesGeo.emplace_back();

                for (const auto& coordinates : feature["geometry"]["coordinates"]) {

                    countriesGeo.back().emplace_back();
                    for (const auto& coord0 : coordinates) {

                        auto &dest = countriesGeo.back().back();
                        if (coord0.is_array() && !coord0.empty() && coord0[0].is_array()) {
                            // multy poligon
                            for (const auto& coord1 : coord0) {
                                double longitude = coord1[0].get<double>();
                                double latitude = coord1[1].get<double>();
                                CartesianCoordinates cartesian = geoToCartesian({ latitude, longitude });

                                dest.emplace_back(cartesian);

                            }
                        } else if (coord0.is_array() && !coord0.empty() && !coord0[0].is_array() && coord0.size() == 2) {

                            for (const auto& coord1 : coordinates) {
                                double longitude = coord1[0].get<double>();
                                double latitude = coord1[1].get<double>();
                                CartesianCoordinates cartesian = geoToCartesian({ latitude, longitude });

                                dest.emplace_back(cartesian);
                            }
                            break;
                        } else {
                            break;
                        }
                    }
                }
            }

        }
    }

    // Create a color map to store a color for each country
    std::map<std::string, ImVec4> countryColors;

    ImVec4 randomColor() {
        static std::random_device rd;
        static std::mt19937 gen(rd());
        std::uniform_real_distribution<> dis(0, 1);

        return ImVec4(dis(gen), dis(gen), dis(gen), 1.0f);
    }

    std::vector<ImVec4> colors = {
        ImVec4(1.0f, 0.0f, 0.0f, 1.0f), // Colors.red
        ImVec4(0.0f, 1.0f, 0.0f, 1.0f), // Colors.green
        ImVec4(1.0f, 0.08f, 0.58f, 1.0f), // Colors.pink
        ImVec4(1.0f, 0.76f, 0.03f, 1.0f), // Colors.amber
        ImVec4(0.0f, 0.0f, 0.0f, 1.0f), // Colors.black
        ImVec4(0.5f, 0.5f, 0.5f, 1.0f), // Colors.grey
        ImVec4(0.6f, 0.32f, 0.17f, 1.0f), // Colors.brown
        ImVec4(1.0f, 0.65f, 0.0f, 1.0f), // Colors.orange
        ImVec4(0.56f, 0.93f, 0.56f, 1.0f) // Colors.lightGreen
    };


    void GeoMap::draw() {

        maybeInit();

        ImDrawList* drawList = ImGui::GetWindowDrawList();
        ImVec2 canvasPos = ImGui::GetCursorScreenPos();
        auto windowWidth = ImGui::GetContentRegionAvail().x;
        auto windowHeight = ImGui::GetContentRegionAvail().y;
        if (windowWidth == 0) {
            return;
        }
        int count = 0;
        int ix = 0;

        for(auto &country : countriesGeo) {
            const ImColor& thisColor = ImColor(colors[(++count) % colors.size()]);
            for(auto &polygon : country) {
                if (polygon.size() > 1) {
                    for (int i = 0; i < polygon.size() - 1; i++) {
                        auto& cartesian = polygon[i];
                        auto& cartesian2 = polygon[i + 1];
                        auto x1 = static_cast<float>(cartesian.x * windowWidth / 2 + windowWidth / 2);
                        auto y1 = static_cast<float>(windowHeight - (cartesian.y * windowHeight / 2 + windowHeight / 2));
                        auto x2 = static_cast<float>(cartesian2.x * windowWidth / 2 + windowWidth / 2);
                        auto y2 = static_cast<float>(windowHeight - (cartesian2.y * windowHeight / 2 + windowHeight / 2));
                        drawList->AddLine(canvasPos + ImVec2(x1, y1), canvasPos + ImVec2(x2, y2), thisColor, 1.0f);
                    }
                }
            }

        }
    };
};