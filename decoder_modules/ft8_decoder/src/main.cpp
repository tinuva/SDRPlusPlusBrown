#include <imgui.h>
#include <config.h>
#include <core.h>
#include <gui/style.h>
#include <gui/gui.h>
#include <signal_path/signal_path.h>
#include <module.h>
#include <filesystem>
#include <dsp/stream.h>
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

#define INPUT_SAMPLE_RATE 14400

class FT8DecoderModule : public ModuleManager::Instance {

    dsp::stream<dsp::complex_t> iqdata;
    std::atomic_bool running;

    const int CAPTURE_SAMPLE_RATE = 12000;

public:
    int rangeContainsInclusive(double largeRangeStart, double largeRangeEnd, double smallRangeStart, double smallRangeEnd) {
        if (largeRangeStart <= smallRangeStart && largeRangeEnd >= smallRangeEnd) {
            return 1;
        }
        return 0;
    }

    const int INVALID_OFFSET = 0x7FFFFFFF;
    const int VFO_BANDWIDTH = 6000;

    int calculateVFOOffset(double centerFrequency, double bandwidth) {
        int rangeStart = centerFrequency - bandwidth;
        int rangeEnd = centerFrequency + bandwidth;
        const int frequencies[] = {1840000, 3573000, 7074000, 10136000, 14074000, 18100000, 21074000, 24915000, 28074000, 50000000};
        for(auto q = std::begin(frequencies); q != std::end(frequencies); q++) {
            auto center = (*q + VFO_BANDWIDTH/2);  // 1500 is the offset of the center of the band
            if (rangeContainsInclusive(rangeStart, rangeEnd, (double)(center - VFO_BANDWIDTH / 2), (double(center + VFO_BANDWIDTH / 2)))) {
                return center - centerFrequency;
            }
        }
        return INVALID_OFFSET;
    }

    EventHandler<double> iqSampleRateListener;

    FT8DecoderModule(std::string name) {
        this->name = name;

        // Load config
        config.acquire();
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

        vfo = new dsp::channel::RxVFO(&iqdata, sigpath::iqFrontEnd.getEffectiveSamplerate(), CAPTURE_SAMPLE_RATE, CAPTURE_SAMPLE_RATE, vfoOffset);

        sigpath::iqFrontEnd.onEffectiveSampleRateChange.bindHandler(&iqSampleRateListener);
        iqSampleRateListener.ctx = this;
        iqSampleRateListener.handler = [](double newSampleRate, void* ctx) {
            spdlog::info("FT8 decoder: effective sample rate changed to {}", newSampleRate);
            ((FT8DecoderModule*)ctx)->vfo->setInSamplerate(newSampleRate);
        };


        usbDemod.init(name, &usbDemodConfig, ifChain.out, 3000, CAPTURE_SAMPLE_RATE);
        ifChain.setInput(&vfo->out, [&](auto ifchainOut){
            usbDemod.setInput(ifchainOut);
            spdlog::info("ifchain change out");
            // next
        });


        std::thread reader([&](){
            auto _this = this;
            std::vector<dsp::stereo_t> reader;
            double ADJUST_PERIOD = 0.1; // seconds before adjusting the vfo offset after user changed the center freq
            int beforeAdjust = (int)(CAPTURE_SAMPLE_RATE * ADJUST_PERIOD);
            while(running.load()) {
                int rd = usbDemod.getOutput()->read();
                if (rd < 0) {
                    break;
                }
                beforeAdjust -= rd;
                if (beforeAdjust <= 0) {
                    beforeAdjust = (int)(CAPTURE_SAMPLE_RATE * ADJUST_PERIOD);
                    int newOffset = calculateVFOOffset(gui::waterfall.getCenterFrequency(), gui::waterfall.getBandwidth());
                    if (newOffset == INVALID_OFFSET) {
                        onTheFrequency = false;
                    } else {
                        onTheFrequency = true;
                        if (newOffset == (int)vfoOffset) {
                            // do nothing
                        } else {
                            vfoOffset = newOffset;
                            vfo->setOffset(vfoOffset-3000);
                        }
                    }
                }

                if (enabled && onTheFrequency) {
                    reader.resize(rd);
                    std::copy(usbDemod.getOutput()->readBuf, usbDemod.getOutput()->readBuf + rd, reader.begin());
                    handleIFData(reader);
                }
                usbDemod.getOutput()->flush();
            }
        });
        reader.detach();

        enable();
    }

    dsp::chain<dsp::complex_t> ifChain;
    dsp::channel::RxVFO* vfo;
    demod::USB usbDemod;
    ConfigManager usbDemodConfig;

    long long prevBlockNumber = 0;
    std::vector<dsp::stereo_t> fullBlock;
    std::mutex processingBlockMutex;
    double vfoOffset = 0.0;
    std::atomic_int blockProcessorsRunning = 0;

    void handleIFData(const std::vector<dsp::stereo_t> &data) {
        long long int curtime = sigpath::iqFrontEnd.getCurrentStreamTime();
        long long blockNumber = (curtime / 1000) / 15;
        long long blockTime = (curtime / 1000) % 15;
        if (blockNumber != prevBlockNumber) {
            bool shouldStartProcessing = false;
            std::shared_ptr<std::vector<dsp::stereo_t>> processingBlock;
            processingBlock = std::make_shared<std::vector<dsp::stereo_t>>(fullBlock);
            shouldStartProcessing = true;
            if (blockProcessorsRunning > 0) {
                shouldStartProcessing = false;
            }
            if (shouldStartProcessing) {
                // no processing is done.
                if (processingBlock->size() / CAPTURE_SAMPLE_RATE > 16 || processingBlock->size() / CAPTURE_SAMPLE_RATE <= 14) {
                    spdlog::info("Block size is not matching");
                    processingBlock.reset();    // clear for new one
                } else {
                    // block size is ok.
                    startBlockProcessing(processingBlock, prevBlockNumber, vfoOffset + gui::waterfall.getCenterFrequency());
                }
            }
            prevBlockNumber = blockNumber;
            fullBlock.clear();
        }
        fullBlock.insert(std::end(fullBlock), std::begin(data), std::end(data));
//        spdlog::info("{} Got {} samples: {}", blockNumber, data.size(), data[0].amplitude());
    }

    void startBlockProcessing(const std::shared_ptr<std::vector<dsp::stereo_t>> &block, int blockNumber, int originalOffset) {
        blockProcessorsRunning.fetch_add(1);
        std::thread processor([=](){
            std::time_t bst = blockNumber * 15;
            spdlog::info("Start processing block, size={}, block time: {}", block->size(), std::asctime(std::gmtime(&bst)));

            dsp::ft8::decodeFT8(CAPTURE_SAMPLE_RATE, block->data(), block->size(), [=](int mode, QStringList result) {
                int smallOffset = atoi(result[7].str->c_str());
                ImGui::DecodedResult decodedResult(ImGui::DM_FT8, ((long long)blockNumber) * 15 * 1000 + 13000, originalOffset + smallOffset, *result[4].str, *result[4].str);
                gui::waterfall.addDecodedResult(decodedResult);
                return;
            });

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
            usbDemod.start();
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
            usbDemod.stop();
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

        /*
        float menuWidth = ImGui::GetContentRegionAvail().x;

        if (!_this->enabled) { style::beginDisabled(); }

        ImGui::SetNextItemWidth(menuWidth);
        _this->diag.draw();

        {
            std::lock_guard lck(_this->lsfMtx);

            auto now = std::chrono::high_resolution_clock::now();
            if (std::chrono::duration_cast<std::chrono::milliseconds>(now - _this->lastUpdated).count() > 1000) {
                _this->lsf.valid = false;
            }

            ImGui::BeginTable(CONCAT("##ft8_info_tbl_", _this->name), 2, ImGuiTableFlags_SizingFixedFit | ImGuiTableFlags_RowBg | ImGuiTableFlags_Borders);
            if (!_this->lsf.valid) {
                ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted("Source");
                ImGui::TableSetColumnIndex(1);
                ImGui::TextUnformatted("--");

                ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted("Destination");
                ImGui::TableSetColumnIndex(1);
                ImGui::TextUnformatted("--");

                ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted("Data Type");
                ImGui::TableSetColumnIndex(1);
                ImGui::TextUnformatted("--");

                ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted("Encryption");
                ImGui::TableSetColumnIndex(1);
                ImGui::TextUnformatted("-- (Subtype --)");

                ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted("CAN");
                ImGui::TableSetColumnIndex(1);
                ImGui::TextUnformatted("--");
            }
            else {
                ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted("Source");
                ImGui::TableSetColumnIndex(1);
                ImGui::TextUnformatted(_this->lsf.src.c_str());

                ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted("Destination");
                ImGui::TableSetColumnIndex(1);
                ImGui::TextUnformatted(_this->lsf.dst.c_str());

                ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted("Data Type");
                ImGui::TableSetColumnIndex(1);
                ImGui::TextUnformatted(FT8DataTypesTxt[_this->lsf.dataType]);

                ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted("Encryption");
                ImGui::TableSetColumnIndex(1);
                ImGui::Text("%s (Subtype %d)", FT8EncryptionTypesTxt[_this->lsf.encryptionType], _this->lsf.encryptionSubType);

                ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted("CAN");
                ImGui::TableSetColumnIndex(1);
                ImGui::Text("%d", _this->lsf.channelAccessNum);
            }
            ImGui::EndTable();
        }

        if (ImGui::Checkbox(CONCAT("Show Reference Lines##ft8_showlines_", _this->name), &_this->showLines)) {
            if (_this->showLines) {
                _this->diag.lines.push_back(-0.75f);
                _this->diag.lines.push_back(-0.25f);
                _this->diag.lines.push_back(0.25f);
                _this->diag.lines.push_back(0.75f);
            }
            else {
                _this->diag.lines.clear();
            }
            config.acquire();
            config.conf[_this->name]["showLines"] = _this->showLines;
            config.release(true);
        }

        if (!_this->enabled) { style::endDisabled(); }

         */
    }

    std::string name;
    bool enabled = false;


    dsp::FT8Decoder decoder;

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
