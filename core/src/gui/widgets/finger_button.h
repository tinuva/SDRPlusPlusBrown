#pragma once

#include <imgui.h>
#include <string>
#include <gui/style.h>

inline bool doFingerButton(const std::string &title) {
    const ImVec2& labelWidth = ImGui::CalcTextSize(title.c_str(), nullptr, true, -1);
    if (title[0] == '>') {
        ImGui::PushStyleColor(ImGuiCol_Text, ImVec4(0.0f, 1.0f, 0.0f, 1.0f));
    }
    auto rv = ImGui::Button(title.c_str(), ImVec2(labelWidth.x + style::baseFont->FontSize, style::baseFont->FontSize * 3));
    if (title[0] == '>') {
        ImGui::PopStyleColor();
    }
    return rv;
};

