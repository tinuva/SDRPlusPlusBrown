#include <imgui.h>
#include <imgui.h>
#include <config.h>
#include <core.h>
#include <gui/style.h>
#include <gui/gui.h>
#include <signal_path/signal_path.h>
#include <module.h>
#include <filesystem>
#include <dsp/stream.h>
#include <dsp/types.h>
#include <dsp/buffer/reshaper.h>
#include <dsp/multirate/rational_resampler.h>
#include <dsp/sink/handler_sink.h>
#include <gui/widgets/folder_select.h>
#include <gui/widgets/symbol_diagram.h>
#include <fstream>
#include <chrono>
#include "ft8_decoder.h"
#include "../../radio/src/demodulators/usb.h"

#define CONCAT(a, b) ((std::string(a) + b).c_str())

SDRPP_MOD_INFO{
    /* Name:            */ "ft8_decoder",
    /* Description:     */ "FT8 Decoder for SDR++",
    /* Author:          */ "FT8 fathers and then I added few lines",
    /* Version:         */ 0, 1, 0,
    /* Max instances    */ -1
};

ConfigManager config;
CTY cty;

#define INPUT_SAMPLE_RATE 14400

class FT8DecoderModule : public ModuleManager::Instance {

    dsp::stream<dsp::complex_t> iqdata;
    std::atomic_bool running;

    const int VFO_SAMPLE_RATE = 24000;
    //    const int CAPTURE_SAMPLE_RATE = 12000;

public:
    int rangeContainsInclusive(double largeRangeStart, double largeRangeEnd, double smallRangeStart, double smallRangeEnd) {
        if (largeRangeStart <= smallRangeStart && largeRangeEnd >= smallRangeEnd) {
            return 1;
        }
        return 0;
    }

    const int INVALID_OFFSET = 0x7FFFFFFF;
    const int USB_BANDWIDTH = 3000;

    int calculateVFOCenterOffset(double centerFrequency, double ifBandwidth) {
        int rangeStart = centerFrequency - ifBandwidth;
        int rangeEnd = centerFrequency + ifBandwidth;
        const int frequencies[] = { 1840000, 3573000, 7074000, 10136000, 14074000, 18100000, 21074000, 24915000, 28074000, 50000000 };
        for (auto q = std::begin(frequencies); q != std::end(frequencies); q++) {
            auto center = (*q + USB_BANDWIDTH);
            if (rangeContainsInclusive(rangeStart, rangeEnd, (double)(center - USB_BANDWIDTH), (double(center + USB_BANDWIDTH)))) {
                return center - centerFrequency;
            }
        }
        return INVALID_OFFSET;
    }

    EventHandler<double> iqSampleRateListener;

    FT8DecoderModule(std::string name) {
        this->name = name;

        //        mshv_init();

        // Load config
        config.acquire();
        if (config.conf[name].find("myGrid") != config.conf[name].end()) {
            auto qq = config.conf[name]["myGrid"].get<std::string>();
            strcpy(myGrid, qq.data());
            calculateMyLoc();
        } else {
            myGrid[0] = 0;
        }
        if (config.conf[name].find("myCallsign") != config.conf[name].end()) {
            auto qq = config.conf[name]["myCallsign"].get<std::string>();
            strcpy(myCallsign, qq.data());
        } else {
            myCallsign[0] = 0;
        }
        if (config.conf[name].find("processingEnabledFT8") != config.conf[name].end()) {
            processingEnabledFT8 = config.conf[name]["processingEnabledFT8"].get<bool>();
        }
        if (config.conf[name].find("processingEnabledFT4") != config.conf[name].end()) {
            processingEnabledFT4 = config.conf[name]["processingEnabledFT4"].get<bool>();
        }
        if (config.conf[name].find("enablePSKReporter") != config.conf[name].end()) {
            enablePSKReporter = config.conf[name]["enablePSKReporter"].get<bool>();
        }
        config.release(true);
        //        if (!config.conf.contains(name)) {
        //            config.conf[name]["showLines"] = false;
        //        }
        //        showLines = config.conf[name]["showLines"];
        //        if (showLines) {
        //            diag.lines.push_back(-0.75f);
        //            diag.lines.push_back(-0.25f);
        //            diag.lines.push_back(0.25f);
        //            diag.lines.push_back(0.75f);
        //        }
        config.release(true);

        running = true;

        //        // Initialize DSP here
        //        decoder.init(vfo->output, INPUT_SAMPLE_RATE, lsfHandler, this);
        //        resamp.init(decoder.out, 8000, audioSampRate);
        //        reshape.init(decoder.diagOut, 480, 0);
        //        diagHandler.init(&reshape.out, _diagHandler, this);
        //
        //        // Start DSO Here
        //        decoder.start();
        //        resamp.start();
        //        reshape.start();
        //        diagHandler.start();
        //
        //        // Setup audio stream
        //        srChangeHandler.ctx = this;
        //        srChangeHandler.handler = sampleRateChangeHandler;
        //        stream.init(&srChangeHandler, audioSampRate);
        //        sigpath::sinkManager.registerStream(name, &stream);
        //
        //        stream.start();

        gui::menu.registerEntry(name, menuHandler, this, this);

        vfo = new dsp::channel::RxVFO(&iqdata, sigpath::iqFrontEnd.getEffectiveSamplerate(), VFO_SAMPLE_RATE, USB_BANDWIDTH, vfoOffset);

        sigpath::iqFrontEnd.onEffectiveSampleRateChange.bindHandler(&iqSampleRateListener);
        iqSampleRateListener.ctx = this;
        iqSampleRateListener.handler = [](double newSampleRate, void* ctx) {
            spdlog::info("FT8 decoder: effective sample rate changed to {}", newSampleRate);
            ((FT8DecoderModule*)ctx)->vfo->setInSamplerate(newSampleRate);
        };

        usbDemod = std::make_shared<demod::USB>();
        usbDemodConfig.acquire();
        usbDemodConfig.disableAutoSave();
        usbDemodConfig.conf[name]["USB"]["agcAttack"] = 0.0;
        usbDemodConfig.conf[name]["USB"]["agcDecay"] = 0.0;
        usbDemodConfig.release(true);
        usbDemod->init(name, &usbDemodConfig, ifChain.out, USB_BANDWIDTH, VFO_SAMPLE_RATE);
        //        usbDemod->setFrozen(true);
        ifChain.setInput(&vfo->out, [&](auto ifchainOut) {
            usbDemod->setInput(ifchainOut);
            spdlog::info("ifchain change out");
            // next
        });


        std::thread reader([&]() {
            auto _this = this;
            std::vector<dsp::stereo_t> reader;
            double ADJUST_PERIOD = 0.1; // seconds before adjusting the vfo offset after user changed the center freq
            int beforeAdjust = (int)(VFO_SAMPLE_RATE * ADJUST_PERIOD);
            while (running.load()) {
                int rd = usbDemod->getOutput()->read();
                if (rd < 0) {
                    break;
                }
                beforeAdjust -= rd;
                if (beforeAdjust <= 0) {
                    beforeAdjust = (int)(VFO_SAMPLE_RATE * ADJUST_PERIOD);
                    int newOffset = calculateVFOCenterOffset(gui::waterfall.getCenterFrequency(), gui::waterfall.getBandwidth());
                    if (newOffset == INVALID_OFFSET) {
                        onTheFrequency = false;
                    }
                    else {
                        onTheFrequency = true;
                        if (newOffset == (int)vfoOffset) {
                            // do nothing
                        }
                        else {
                            vfoOffset = newOffset;
                            spdlog::info("FT8 vfo: center offset {}, bandwidth: {}", vfoOffset, USB_BANDWIDTH);
                            vfo->setOffset(vfoOffset - USB_BANDWIDTH / 2);
                        }
                    }
                }

                if (enabled && onTheFrequency) {
                    reader.resize(rd);
                    std::copy(usbDemod->getOutput()->readBuf, usbDemod->getOutput()->readBuf + rd, reader.begin());
                    handleIFData(reader);
                }
                usbDemod->getOutput()->flush();
            }
        });
        reader.detach();

        enable();
    }

    dsp::chain<dsp::complex_t> ifChain;
    dsp::channel::RxVFO* vfo;
    ConfigManager usbDemodConfig;

    long long prevBlockNumber = 0;
    std::shared_ptr<std::vector<dsp::stereo_t>> fullBlock;
    std::mutex processingBlockMutex;
    double vfoOffset = 0.0;
    std::atomic_int blockProcessorsRunning = 0;

    void handleIFData(const std::vector<dsp::stereo_t>& data) {
        long long int curtime = sigpath::iqFrontEnd.getCurrentStreamTime();
        long long blockNumber = (curtime / 1000) / 15;
        long long blockTime = (curtime / 1000) % 15;
        if (blockNumber != prevBlockNumber) {
            if (fullBlock) {
                bool shouldStartProcessing = false;
                std::shared_ptr<std::vector<dsp::stereo_t>> processingBlock;
                processingBlock = fullBlock;
                shouldStartProcessing = true;
                if (blockProcessorsRunning > 0) {
                    shouldStartProcessing = false;
                }
                if (shouldStartProcessing) {
                    // no processing is done.
                    if (processingBlock->size() / VFO_SAMPLE_RATE > 16 || processingBlock->size() / VFO_SAMPLE_RATE <= 13) {
                        spdlog::info("Block size is not matching: {}, curtime={}",
                                     processingBlock->size() / VFO_SAMPLE_RATE,
                                     curtime);
                        processingBlock.reset(); // clear for new one
                    }
                    else {
                        // block size is ok.
                        startBlockProcessing(processingBlock, prevBlockNumber, vfoOffset + gui::waterfall.getCenterFrequency());
                    }
                }
                fullBlock.reset();
            }
            prevBlockNumber = blockNumber;
        }
        if (!fullBlock) {
            fullBlock = std::make_shared<std::vector<dsp::stereo_t>>();
            fullBlock->reserve(16 * VFO_SAMPLE_RATE);
        }
        fullBlock->insert(std::end(*fullBlock), std::begin(data), std::end(data));
        //        spdlog::info("{} Got {} samples: {}", blockNumber, data.size(), data[0].l);
    }

    std::shared_ptr<demod::USB> usbDemod;

    void startBlockProcessing(const std::shared_ptr<std::vector<dsp::stereo_t>>& block, int blockNumber, int originalOffset) {
        blockProcessorsRunning.fetch_add(1);
        std::thread processor([=]() {
            std::time_t bst = blockNumber * 15;
            spdlog::info("Start processing block, size={}, block time: {}", block->size(), std::asctime(std::gmtime(&bst)));

            std::mutex mtx;

            auto handler = [=](int mode, std::vector<std::string> result) {
                int smallOffset = atoi(result[7].c_str());
                auto callsign = extractCallsignFromFT8(result[4]);
                if (callsign.empty()) {
                    callsign = "?? " + result[4];
                } else {
                    auto cs = cty.findCallsign(callsign);
                    if (!cs.value.empty()) {
                        callsign += " " + cs.dxccname;
                    }
                    if (myPos.isValid()) {
                        auto bd = bearingDistance(myPos, cs.ll);
                        callsign += " " + std::to_string((int)bd.distance)+"km";
                    }
                }
                ImGui::DecodedResult decodedResult(ImGui::DM_FT8, ((long long)blockNumber) * 15 * 1000 + 15000, originalOffset, callsign, result[4]);
                auto strength = atof(result[1].c_str());
                strength = (strength + 24) / (24 + 24);
                if (strength < 0.0) strength = 0.0;
                if (strength > 1.0) strength = 1.0;
                decodedResult.strength = strength;
                decodedResult.intensity = (random() % 100) / 100.0;
                gui::waterfall.addDecodedResult(decodedResult);
                return;
            };
            std::thread t0([=]() {
                dsp::ft8::decodeFT8(VFO_SAMPLE_RATE, block->data(), block->size(), handler);
            });
            //            std::thread t1([=]() {
            //                dsp::ft8::decodeFT8(CAPTURE_SAMPLE_RATE, block->data() + CAPTURE_SAMPLE_RATE, block->size() - CAPTURE_SAMPLE_RATE, handler);
            //            });
            //            std::thread t2([=]() {
            //                dsp::ft8::decodeFT8(CAPTURE_SAMPLE_RATE, block->data() + 2 * CAPTURE_SAMPLE_RATE, block->size() - 2 * CAPTURE_SAMPLE_RATE, handler);
            //            });
            //            std::thread t3([=]() {
            //                dsp::ft8::decodeFT8(CAPTURE_SAMPLE_RATE, block->data() + 3 * CAPTURE_SAMPLE_RATE, block->size() - 3 * CAPTURE_SAMPLE_RATE, handler);
            //            });
            //            t3.join();
            //            t2.join();
            //            t1.join();
            t0.join();

            blockProcessorsRunning.fetch_add(-1);
        });
        processor.detach();
    }

    bool onTheFrequency = false;

    ~FT8DecoderModule() {
        sigpath::iqFrontEnd.onEffectiveSampleRateChange.unbindHandler(&iqSampleRateListener);
        ifChain.out->stopReader();
        running = false;

        gui::menu.removeEntry(name);
        // Stop DSP Here
        //        stream.stop();
        if (enabled) {
            //            decoder.stop();
            //            resamp.stop();
            //            reshape.stop();
            //            diagHandler.stop();
            //            sigpath::vfoManager.deleteVFO(vfo);
        }

        //        sigpath::sinkManager.unregisterStream(name);
    }

    void postInit() {}

    void enable() {
        if (!enabled) {
            sigpath::iqFrontEnd.bindIQStream(&iqdata);
            vfo->start();
            ifChain.start();
            usbDemod->start();
            enabled = true;
            spdlog::info("FT8 Decoder enabled");
        }
    }

    void disable() {
        // Stop DSP here
        //        decoder.stop();
        //        resamp.stop();
        //        reshape.stop();
        //        diagHandler.stop();
        //
        //        sigpath::vfoManager.deleteVFO(vfo);

        if (enabled) {
            usbDemod->stop();
            ifChain.stop();
            vfo->stop();
            sigpath::iqFrontEnd.unbindIQStream(&iqdata);
            enabled = false;
            spdlog::info("FT8 Decoder disabled");
        }
    }

    bool isEnabled() {
        return enabled;
    }

private:
    static void menuHandler(void* ctx) {
        FT8DecoderModule* _this = (FT8DecoderModule*)ctx;
        ImGui::LeftLabel("My Grid");
        ImGui::FillWidth();
        if (ImGui::InputText(CONCAT("##_my_grid_", _this->name), _this->myGrid, 8)) {
            config.acquire();
            config.conf[_this->name]["myGrid"] = _this->myGrid;
            config.release(true);
            _this->calculateMyLoc();
        }
        ImGui::LeftLabel("My Loc: ");
        ImGui::FillWidth();
        if (_this->myPos.isValid()) {
            ImGui::Text("%+02.5f %+02.5f", _this->myPos.lat, _this->myPos.lon);
        } else {
            ImGui::Text("Invalid");
        }
        ImGui::LeftLabel("My Callsign");
        ImGui::FillWidth();
        if (ImGui::InputText(CONCAT("##_my_callsign_", _this->name), _this->myCallsign, 12)) {
            config.acquire();
            config.conf[_this->name]["myCallsign"] = _this->myCallsign;
            config.release(true);
        }

        ImGui::LeftLabel("Decode FT8");
        ImGui::FillWidth();
        if (ImGui::Checkbox(CONCAT("##_processing_enabled_ft8_", _this->name), &_this->processingEnabledFT8)) {
            config.acquire();
            config.conf[_this->name]["processingEnabledFT8"] = _this->processingEnabledFT8;
            config.release(true);
        }
        ImGui::LeftLabel("Decode FT4");
        ImGui::FillWidth();
        if (ImGui::Checkbox(CONCAT("##_processing_enabled_ft4_", _this->name), &_this->processingEnabledFT4)) {
            config.acquire();
            config.conf[_this->name]["processingEnabledFT4"] = _this->processingEnabledFT4;
            config.release(true);
        }
        ImGui::LeftLabel("PskReporter");
        ImGui::FillWidth();
        if (ImGui::Checkbox(CONCAT("##_enable_psk_reporter_", _this->name), &_this->enablePSKReporter)) {
            config.acquire();
            config.conf[_this->name]["enablePSKReporter"] = _this->enablePSKReporter;
            config.release(true);
        }
    }

    void calculateMyLoc() {
        auto ll = gridToLatLng(myGrid);
        if (ll.isValid()) {
            myPos = ll;
        }
    }

    std::string name;
    bool enabled = false;
    char myGrid[10];
    char myCallsign[13];
    LatLng myPos = LatLng::invalid();
    bool processingEnabledFT8 = true;
    bool processingEnabledFT4 = true;
    bool enablePSKReporter = true;


    //    dsp::buffer::Reshaper<float> reshape;
    //    dsp::sink::Handler<float> diagHandler;
    //    dsp::multirate::RationalResampler<dsp::stereo_t> resamp;
    //

    std::chrono::time_point<std::chrono::high_resolution_clock> lastUpdated;
};

MOD_EXPORT void _INIT_() {
    // Create default recording directory
    json def = json({});
    config.setPath(core::args["root"].s() + "/ft8_decoder_config.json");
    config.load(def);
    config.enableAutoSave();
    std::string resDir = core::configManager.conf["resourcesDirectory"];
    auto path = resDir + "/cty.dat";
    loadCTY(path, cty);
}

MOD_EXPORT ModuleManager::Instance* _CREATE_INSTANCE_(std::string name) {
    return new FT8DecoderModule(name);
}

MOD_EXPORT void _DELETE_INSTANCE_(void* instance) {
    delete (FT8DecoderModule*)instance;
}

MOD_EXPORT void _END_() {
    config.disableAutoSave();
    config.save();
}
