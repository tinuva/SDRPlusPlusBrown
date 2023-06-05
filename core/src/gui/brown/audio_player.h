#pragma once

#include <imgui_internal.h>
#include <imgui.h>
#include <string>
#include <dsp/stream.h>
#include <dsp/types.h>

struct AudioPlayer {
    std::string id;
    std::string error;
    float position;
    bool playing;
    int64_t dataPosition;
    dsp::stream<dsp::stereo_t> outStream;
    std::vector<dsp::stereo_t> *data;
    std::vector<dsp::stereo_t> dataOwn;
    AudioPlayer(const std::string &id) {
        this->id = id;
        position = 0;
    }
    void loadFile(const std::string &path);
    void setData(std::vector<dsp::stereo_t> *data, uint32_t sampleRate);
    void draw();
    void startPlaying();
    uint32_t sampleRate;
};