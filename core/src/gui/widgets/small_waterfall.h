#pragma once

#include <memory>
#include <string>
#include <imgui.h>
#include <dsp/types.h>

struct SubWaterfall {

    struct SubWaterfallPrivate;


    std::shared_ptr<SubWaterfallPrivate> pvt;

    SubWaterfall(int sampleRate, const std::string &);
    ~SubWaterfall();
    void init();
    void draw(ImVec2 loc, ImVec2 wfSize);
    void addAudioSamples(dsp::stereo_t * samples, int count, int sampleRate);

};