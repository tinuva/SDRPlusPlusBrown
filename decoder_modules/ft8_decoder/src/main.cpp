#define IMGUI_DEFINE_MATH_OPERATORS
#define _USE_MATH_DEFINES
#include <imgui.h>
#include <imgui.h>
#include <config.h>
#include <core.h>
#include <gui/style.h>
#include <gui/gui.h>
#include <gui/widgets/waterfall.h>
#include <signal_path/signal_path.h>
#include <module.h>
#include <filesystem>
#include <dsp/stream.h>
#include <dsp/types.h>
#include <gui/widgets/folder_select.h>
#include <fstream>
#include <chrono>
#include "ft8_decoder.h"
#include "../../radio/src/demodulators/usb.h"
#include <utils/kmeans.h>


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

enum DecodedMode {
    DM_FT8 = 1,
    DM_FT4 = 2
};

static ImVec2 baseTextSize;


struct CallHashCache {

    struct CallWithTS {
        std::string call;
        int hash10;
        int hash12;
        int hash22;
        long long lastseen;
    };

    std::vector<CallWithTS> calls;

    long long lastctm = 0;

    void addCall(std::string call, long long ctm) {     // call without < .. >
        if (call.length() < 3) {        // CQ, DE etc
            return;
        }
        call = trim(call);
        if (call.find(' ') != std::string::npos) {  // CQ DX etc
            return;
        }
        if (call.find('<') != std::string::npos) {  // <1:2983> or <...>
            return;
        }
        call = normcall(call);
        for (auto &c : calls) {
            if (c.call == call) {
                c.lastseen = ctm;
                return;
            }
        }
        calls.emplace_back(CallWithTS{call, ihashcall(call, 10), ihashcall(call, 12), ihashcall(call, 22), ctm});
        if (lastctm != ctm) {
            calls.erase(std::remove_if(calls.begin(), calls.end(), [ctm](const CallWithTS &c) { return ctm - c.lastseen > 180000; }), calls.end());
            lastctm = ctm;
        }
    }

    std::string findCall(const std::string &hashed, long long ctm) { // hashed: <1:2983> or <2:2983> or <0:3498>
        if (hashed.size() == 0) {
            return "";
        }
        if (hashed.size() < 3) {
            return hashed;
        }
        if (hashed.front() == '<' && hashed.back() == '>' && hashed[2] == ':' && hashed[1] >= '1' && hashed[1] <= '3') {
            int hash = std::stoi(hashed.substr(3, hashed.size()-4));
            int mode = hashed[1] - '0';
            for (auto &c : calls) {
                if (mode == 0 && c.hash10 == hash) {
                    c.lastseen = ctm;
                    return trim(c.call);
                }
                if (mode == 1 && c.hash12 == hash) {
                    c.lastseen = ctm;
                    return trim(c.call);
                }
                if (mode == 2 && c.hash22 == hash) {
                    c.lastseen = ctm;
                    return trim(c.call);
                }
            }
        }
        return hashed; // not found
    }

    std::string trim(std::string in) {
        while (in.size() > 0 && in[0] == ' ') {
            in.erase(0, 1);
        }
        while (in.size() > 0 && in[in.size()-1] == ' ') {
            in.erase(in.end() - 1);
        }
        return in;
    }

    std::string normcall(std::string call) {
        while(call.size() > 0 && call[0] == ' ')
            call.erase(0, 1);
        while(call.size() > 0 && call[call.size()-1] == ' ')
            call.erase(call.end() - 1);
        while(call.size() < 11)
            call += " ";
        return call;
    }


    // via https://github.com/rtmrtmrtmrtm/ft8mon/blob/master/unpack.cc
    int ihashcall(const std::string &call, int m)
    {
        const char *chars = " 0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ/";

        unsigned long long x = 0;
        for(int i = 0; i < 11; i++){
            int c = call[i];
            const char *p = strchr(chars, c);
            assert(p);
            int j = p - chars;
            x = 38*x + j;
        }
        x = x * 47055833459LL;
        x = x >> (64 - m);

        return x;
    }
};


struct DecodedResult {
    DecodedMode mode;
    long long decodeEndTimestamp;
    long long frequency;
    std::string shortString;
    // above is unique key

    DecodedResult(DecodedMode mode, long long int decodeEndTimestamp, long long int frequency, const std::string& shortString, const std::string& detailedString);

    bool operator == (const DecodedResult&another) const {
        return mode == another.mode && decodeEndTimestamp == another.decodeEndTimestamp && frequency == another.frequency && shortString == another.shortString;
    }

    std::string detailedString;
    double strength; // normalized 0..1.0
    double strengthRaw = 0;

    // below, zero means not layed out.
    double width = -1;
    double height = -1;
    double group = 0;
    double intensity = 0;
    double distance = 0;
    std::string qth = "";
    long long addedTime = 0;

};


struct DrawableDecodedResult {
    virtual long long getDecodeEndTimestamp() = 0;
    virtual long long getFrequency() = 0;
    double layoutY = 0;     // y on the waterfall relative to decodeEndTimestamp, in pixels;
    double layoutX = 0;     // x on the waterfall relative its to frequency (after rescale, relayout needed), in pixels
    const char *info;
    virtual void draw(const ImVec2& origin, ImGuiWindow* pWindow) = 0;
};

struct FT8DrawableDecodedResult : DrawableDecodedResult {

    DecodedResult* result;
    FT8DrawableDecodedResult(DecodedResult* result);
    long long int getDecodeEndTimestamp() override;
    long long int getFrequency() override;
    void draw(const ImVec2& origin, ImGuiWindow* pWindow) override;
};

struct FT8DrawableDecodedDistRange : DrawableDecodedResult {
    std::string extraText;
    long long freq;
    explicit FT8DrawableDecodedDistRange(const std::string& extraText, long long freq);

    long long int getDecodeEndTimestamp() override;
    long long int getFrequency() override;
    void draw(const ImVec2& origin, ImGuiWindow* pWindow) override;
};

struct KMeansDR {
    double distance;
    int group = 0;
    int drIndex;
    double _kmc;

    KMeansDR(double distance, int drIndex) : distance(distance), drIndex(drIndex), _kmc(log10(distance)) {}

    double kmeansDistanceTo(KMeansDR *another) const {
        return abs(kmeansCoord()-another->kmeansCoord());
    }

    double kmeansCoord() const {
        return _kmc;
    }

    void setKmeansCoord(double coord) {
        _kmc = coord;
    }

};

static const int VFO_SAMPLE_RATE = 24000;
static const int INVALID_OFFSET = 0x7FFFFFFF;
static const int USB_BANDWIDTH = 3000;

static int rangeContainsInclusive(double largeRangeStart, double largeRangeEnd, double smallRangeStart, double smallRangeEnd) {
    if (largeRangeStart <= smallRangeStart && largeRangeEnd >= smallRangeEnd) {
        return 1;
    }
    return 0;
}

static std::pair<int,int> calculateVFOCenterOffset(const std::vector<int> &frequencies, double centerFrequency, double ifBandwidth) {
    int rangeStart = centerFrequency - ifBandwidth;
    int rangeEnd = centerFrequency + ifBandwidth;
    for (auto q = std::begin(frequencies); q != std::end(frequencies); q++) {
        auto center = (*q + USB_BANDWIDTH);
        if (rangeContainsInclusive(rangeStart, rangeEnd, (double)(center - USB_BANDWIDTH), (double(center + USB_BANDWIDTH)))) {
            return std::make_pair(center - centerFrequency, center);
        }
    }
    return std::make_pair(INVALID_OFFSET, INVALID_OFFSET);
}




class FT8DecoderModule;

struct SingleDecoder {
    FT8DecoderModule *mod;
    std::vector<dsp::stereo_t> reader;
    double ADJUST_PERIOD = 0.1; // seconds before adjusting the vfo offset after user changed the center freq
    int beforeAdjust = (int)(VFO_SAMPLE_RATE * ADJUST_PERIOD);
    int previousCenterOffset = 0;
    bool onTheFrequency = false;
    dsp::stream<dsp::complex_t> iqdata;
    std::atomic_bool running;
    bool processingEnabled = true;
    FT8DecoderModule *mod2;

    void handleData(int rd, dsp::stereo_t* inputData);

    virtual void unbind() {
        usbDemod->stop();
        ifChain.stop();
        vfo->stop();
        sigpath::iqFrontEnd.unbindIQStream(&iqdata);
    }

    virtual void bind() {
        sigpath::iqFrontEnd.bindIQStream(&iqdata);
        vfo->start();
        ifChain.start();
        usbDemod->start();

    }

    virtual DecodedMode getModeDM() = 0;

    virtual std::string getModeString() = 0;

    dsp::chain<dsp::complex_t> ifChain;
    dsp::channel::RxVFO* vfo;
    ConfigManager usbDemodConfig;

    double prevBlockNumber = 0;
    std::shared_ptr<std::vector<dsp::stereo_t>> fullBlock;
    std::mutex processingBlockMutex;
    double vfoOffset = 0.0;
    std::atomic_int blockProcessorsRunning = 0;
    std::atomic_int lastDecodeTime0 = 0;
    std::atomic_int lastDecodeTime = 0;
    std::atomic_int lastDecodeCount = 0;
    std::atomic_int totalCallsignsDisplayed = 0;
    std::shared_ptr<demod::USB> usbDemod;
    CallHashCache callHashCache;
    std::mutex callHashCacheMutex;
    EventHandler<double> iqSampleRateListener;
    EventHandler<bool> onPlayStateChange;


    virtual void init(const std::string &name);
    virtual void destroy();
    virtual double getBlockDuration() = 0;

    void handleIFData(const std::vector<dsp::stereo_t>& data) {
        long long int curtime = sigpath::iqFrontEnd.getCurrentStreamTime();
        double blockNumber = floor((curtime / 1000.0) / getBlockDuration());
        if (blockNumber != prevBlockNumber) {
            if (fullBlock) {
                bool shouldStartProcessing = true;
                std::shared_ptr<std::vector<dsp::stereo_t>> processingBlock;
                processingBlock = fullBlock;
                if (blockProcessorsRunning > 0) {
                    shouldStartProcessing = false;
                }
                if (shouldStartProcessing) {
                    // no processing is done.
                    if (processingBlock->size() / VFO_SAMPLE_RATE > getBlockDuration()+1 || processingBlock->size() / VFO_SAMPLE_RATE <= getBlockDuration()-2) {
                        flog::info("Block size ({}) is not matching: {}, curtime={}",
                                     getModeString(),
                                     processingBlock->size() / VFO_SAMPLE_RATE,
                                     curtime);
                        processingBlock.reset(); // clear for new one
                    } else {
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
            fullBlock->reserve((getBlockDuration() + 1) * VFO_SAMPLE_RATE);
        }
        fullBlock->insert(std::end(*fullBlock), std::begin(data), std::end(data));
        //        flog::info("{} Got {} samples: {}", blockNumber, data.size(), data[0].l);
    }

    void startBlockProcessing(const std::shared_ptr<std::vector<dsp::stereo_t>>& block, int blockNumber, int originalOffset);

    virtual std::pair<int,int> calculateVFOCenterOffsetForMode(double centerFrequency, double ifBandwidth) = 0;

};

struct SingleFT8Decoder : SingleDecoder {


    double getBlockDuration() override {
        return 15;
    }

    std::string getModeString() override {
        return "ft8";
    }
    
    DecodedMode getModeDM() override {
        return DM_FT8;
    }
    

    std::pair<int,int> calculateVFOCenterOffsetForMode(double centerFrequency, double ifBandwidth) override {
        static std::vector<int> frequencies = { 1840000, 3573000, 5357000, 7074000, 10136000, 14074000, 18100000, 21074000, 24915000, 28074000, 50000000 };
        return calculateVFOCenterOffset(frequencies, centerFrequency, ifBandwidth);
    }
};

struct SingleFT4Decoder : SingleDecoder {

    SingleFT4Decoder() {
        flog::info("FT4 decoder created");
    }

    double getBlockDuration() override {
        return 15/2.0;
    }

    DecodedMode getModeDM() override {
        return DM_FT4;
    }


    std::string getModeString() override {
        return "ft4";
    }
    std::pair<int,int> calculateVFOCenterOffsetForMode(double centerFrequency, double ifBandwidth) override {
        static std::vector<int> frequencies = { 7047500, 10140000, 14080000, 21140000, 28180000 };
        return calculateVFOCenterOffset(frequencies, centerFrequency, ifBandwidth);
    }


};


class FT8DecoderModule : public ModuleManager::Instance {

    SingleFT8Decoder ft8decoder;
    SingleFT4Decoder ft4decoder;

    std::vector<SingleDecoder *>allDecoders = { &ft8decoder, &ft4decoder };

    //    const int CAPTURE_SAMPLE_RATE = 12000;

public:

    std::vector<DecodedResult>  decodedResults;
    std::vector<std::shared_ptr<DrawableDecodedResult>>  decodedResultsDrawables;
    std::mutex decodedResultsLock;


    void addDecodedResult(const DecodedResult&x) {
        std::lock_guard g(decodedResultsLock);
        for(const auto & decodedResult : decodedResults) {
            if(decodedResult == x) {
                return;
            }
        }
        decodedResults.push_back(x);
        decodedResults.back().addedTime = currentTimeMillis();
        decodedResultsDrawables.clear();
    }

    void clearDecodedResults(DecodedMode mode) {
        std::lock_guard g(decodedResultsLock);
        decodedResults.erase(std::remove_if(decodedResults.begin(), decodedResults.end(), [mode](const DecodedResult& x) { return x.mode == mode; }), decodedResults.end());
        decodedResultsDrawables.clear();
    }


    void drawDecodedResults(const ImGui::WaterFall::WaterfallDrawArgs &args) {

//        auto ctm = currentTimeMillis();
        decodedResultsLock.lock();
//        auto wfHeight = args.wfMax.y - args.wfMin.y;
        auto wfWidth = args.wfMax.x - args.wfMin.x;
        //        auto timeDisplayed = (wfMax.y - wfMin.y) * sigpath::iqFrontEnd.getFFTRate() / waterfallHeight;
        double timePerLine = 1000.0 / sigpath::iqFrontEnd.getFFTRate();
        auto currentTime = sigpath::iqFrontEnd.getCurrentStreamTime();

        const ImVec2 vec2 = ImGui::GetMousePos();


        // delete obsolete ones, and detect relayout
        for(int i=0; i<decodedResults.size(); i++) {
            auto& result = decodedResults[i];
            auto resultTimeDelta = currentTime - result.decodeEndTimestamp;
            if (resultTimeDelta > secondsToKeepResults*1000 + 5000) {
                decodedResults.erase(decodedResults.begin() + i);
                decodedResultsDrawables.clear();
                i--;
                continue;
            }
        }
        if (baseTextSize.y == 0) {
            ImGui::PushFont(style::baseFont);
            baseTextSize = ImGui::CalcTextSize("WW6WWW");
            ImGui::PopFont();
        }
        ImGui::PushFont(style::tinyFont);
        auto modeSize = ImGui::CalcTextSize("ft8");
        ImGui::PopFont();


        if (decodedResults.size() > 0 && decodedResultsDrawables.size() == 0) {

//            KMeans<KMeansDR> kmeans;
//            std::vector<KMeansDR> kmeansData;
//            for(int i=0; i<decodedResults.size(); i++) {
//                double dst = decodedResults[i].distance;
//                if (dst <= 0) {
//                    dst = 1;
//                }
//                kmeansData.emplace_back(KMeansDR(dst, i));
//            }
//            const int MAXGROUPS = 8;
//            int ngroups = std::min<int>(decodedResults.size(), MAXGROUPS);
//            kmeans.lloyd(kmeansData.data(), kmeansData.size(), ngroups, 50);
            static int distances[] = {1000, 2000, 3000, 4000, 5000, 6000, 8000, 10000, 13000, 15000, 18000};
            static auto getGroup = [](int distance) -> int {
                for(int i=0; i<sizeof(distances)/sizeof(distances[0]); i++) {
                    if (distance < distances[i]) {
                        return i;
                    }
                }
                return sizeof(distances)/sizeof(distances[0])-1; // last one
            };
            for(int i=0; i<decodedResults.size(); i++) {
                decodedResults[i].group = getGroup(decodedResults[i].distance);
            }
            auto ngroups = sizeof(distances)/sizeof(distances[0]);
            const int MAXGROUPS = ngroups;
            std::vector<int> groupDists;
            std::vector<bool> foundGroups;
            std::vector<int> groupSortable;
            groupDists.resize(ngroups);
            groupSortable.resize(ngroups);
            foundGroups.resize(ngroups, false);
            for(int i=0; i<decodedResults.size(); i++) {
//                decodedResults[i].group = kmeansData[i].group;
                groupDists[decodedResults[i].group] = decodedResults[i].distance;
                foundGroups[decodedResults[i].group] = true;
            }
            int fgroups = 0;
            for(auto fg: foundGroups) {
                if (fg) {
                    fgroups++;
                }
            }
//            flog::info("Found {} groups among {} results", fgroups, decodedResults.size());

            // sorting groups by distance
            for(int i=0; i<ngroups; i++) {
                groupSortable[i] = i;
            }
            std::sort(groupSortable.begin(), groupSortable.end(), [&groupDists](int a, int b) {
                return groupDists[a] > groupDists[b];
            });
            double scanY = 0;
            double scanX = 0;
            double maxColumnWidth = baseTextSize.x * layoutWidth;

            ft8decoder.totalCallsignsDisplayed = 0;
            ft4decoder.totalCallsignsDisplayed = 0;

            for(int i=0;i<ngroups; i++) {
                std::vector<DecodedResult*>insideGroup;
                for(auto & decodedResult : decodedResults) {
                    if (decodedResult.group == groupSortable[i]) {
                        insideGroup.push_back(&decodedResult);
                    }
                }
                std::sort(insideGroup.begin(), insideGroup.end(), [](DecodedResult *a, DecodedResult *b) {
                    if (a->mode == b->mode) {
                        return strcmp(a->shortString.c_str(), b->shortString.c_str()) < 0;
                    } else {
                        return (int)b->mode < (int)a->mode;
                    }
                });
                std::string lastAdded;
                double maxDistance = 0;
                for(auto & decodedResult : insideGroup) {
                    if (decodedResult->shortString != lastAdded) {
                        maxDistance = std::max<double>(maxDistance, decodedResult->distance);
                        lastAdded = decodedResult->shortString;
                        ImGui::PushFont(style::baseFont);
                        auto tisTextSize = ImGui::CalcTextSize(decodedResult->shortString.c_str());
                        ImGui::PopFont();
                        tisTextSize.x += modeSize.x;
                        if (scanX + tisTextSize.x > maxColumnWidth) {
                            scanX = 0;
                            scanY += baseTextSize.y;
                        }

                        auto fdr = std::make_shared<FT8DrawableDecodedResult>(decodedResult);
                        fdr->layoutX = scanX;
                        fdr->layoutY = scanY;
                        fdr->result->intensity = (1.0 / (MAXGROUPS - 1)) * i;
                        decodedResultsDrawables.emplace_back(fdr);
                        switch(decodedResult->mode) {
                        case DecodedMode::DM_FT8:
                                ft8decoder.totalCallsignsDisplayed++;
                                break;
                            case DecodedMode::DM_FT4:
                                ft4decoder.totalCallsignsDisplayed++;
                                break;
                        }
                        scanX += tisTextSize.x;
                    }

                }
                if (!insideGroup.empty()) {
                    auto freqq = (double)(*insideGroup.begin())->frequency;
                    auto label = "<= " + std::to_string((int)maxDistance) + " KM";
                    if (!myPos.isValid()) {
                        label = "=> setup your GRID SQUARE";
                    }
                    auto fdr2 = std::make_shared<FT8DrawableDecodedDistRange>(label, freqq);
                    fdr2->layoutX = scanX;
                    fdr2->layoutY = scanY;
//                    flog::info("closing: {} {} {}", i, fdr2->layoutX, fdr2->layoutY);
                    decodedResultsDrawables.emplace_back(fdr2);
                    scanX = 0;
                    scanY += baseTextSize.y + baseTextSize.y /2.5;
                }

            }

        }



        std::vector<ImRect> rects;

        // place new ones

        if (!decodedResultsDrawables.empty()) {
            auto mdf = decodedResultsDrawables[0]->getFrequency();
            for (auto& result : decodedResultsDrawables) {
                mdf = std::min<long long>(result->getFrequency(), mdf);
            }
            for (int i = 0; i < decodedResultsDrawables.size(); i++) {
                auto& result = decodedResultsDrawables[i];

                auto df = mdf - gui::waterfall.getCenterFrequency();
                auto dx = (wfWidth / 2) + df / (gui::waterfall.getViewBandwidth() / 2) * (wfWidth / 2); // in pixels, on waterfall

                auto drawX = dx;
                auto drawY = 20;
                const ImVec2 origin = ImVec2(args.wfMin.x + drawX, args.wfMin.y + drawY);

                result->draw(origin, args.window);
            }
        }

        decodedResultsLock.unlock();
    }

    EventHandler<ImGui::WaterFall::WaterfallDrawArgs> afterWaterfallDrawListener;


    FT8DecoderModule(std::string name) {
        this->name = name;
        ft8decoder.mod = ft8decoder.mod2 = this;
        ft4decoder.mod = ft4decoder.mod2 = this;
        gui::waterfall.afterWaterfallDraw.bindHandler(&afterWaterfallDrawListener);
        afterWaterfallDrawListener.ctx = this;
        afterWaterfallDrawListener.handler = [](ImGui::WaterFall::WaterfallDrawArgs args, void* ctx) {
            ((FT8DecoderModule*)ctx)->drawDecodedResults(args);
        };
        decodedResults.reserve(2000);  // to keep addresses constant.

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
        if (config.conf[name].find("layoutWidth") != config.conf[name].end()) {
            layoutWidth = config.conf[name]["layoutWidth"];
        }
        if (config.conf[name].find("secondsToKeepResults") != config.conf[name].end()) {
            secondsToKeepResults = config.conf[name]["secondsToKeepResults"];
        }
        if (config.conf[name].find("processingEnabledFT8") != config.conf[name].end()) {
            ft8decoder.processingEnabled = config.conf[name]["processingEnabledFT8"].get<bool>();
        }
        if (config.conf[name].find("processingEnabledFT4") != config.conf[name].end()) {
            ft4decoder.processingEnabled = config.conf[name]["processingEnabledFT4"].get<bool>();
        }
        if (config.conf[name].find("enablePSKReporter") != config.conf[name].end()) {
            enablePSKReporter = config.conf[name]["enablePSKReporter"].get<bool>();
        }
        config.release(true);

        gui::menu.registerEntry(name, menuHandler, this, this);

        std::for_each(allDecoders.begin(), allDecoders.end(), [&](SingleDecoder* d) {
            d->init(name);
        });

        enable();
    }




    ~FT8DecoderModule() {
        gui::waterfall.afterWaterfallDraw.unbindHandler(&afterWaterfallDrawListener);
        std::for_each(allDecoders.begin(), allDecoders.end(), [&](SingleDecoder* d) {
            d->destroy();
        });

        gui::menu.removeEntry(name);
    }

    void postInit() {}

    void enable() {
        if (!enabled) {
            std::for_each(allDecoders.begin(), allDecoders.end(), [](auto& d) { d->bind(); });
            enabled = true;
            flog::info("FT8 Decoder enabled");
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
            std::for_each(allDecoders.begin(), allDecoders.end(), [](auto& d) { d->unbind(); });
            enabled = false;
            flog::info("FT8 Decoder disabled");
        }
    }

    bool isEnabled() {
        return enabled;
    }

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
        ImGui::LeftLabel("Layout width");
        ImGui::FillWidth();
        if (ImGui::SliderInt("##ft8_layout_width", &_this->layoutWidth, 3, 10, "%d", 0)) {
            config.acquire();
            config.conf[_this->name]["layoutWidth"] = _this->layoutWidth;
            config.release(true);
        }
        ImGui::LeftLabel("Keep results (sec)");
        ImGui::FillWidth();
        if (ImGui::SliderInt("##ft8_keep_results_sec", &_this->secondsToKeepResults, 15, 300, "%d", 0)) {
            config.acquire();
            config.conf[_this->name]["secondsToKeepResults"] = _this->secondsToKeepResults;
            config.release(true);
        }
        ImGui::LeftLabel("Decode FT8");
        if (ImGui::Checkbox(CONCAT("##_processing_enabled_ft8_", _this->name), &_this->ft8decoder.processingEnabled)) {
            config.acquire();
            config.conf[_this->name]["processingEnabledFT8"] = _this->ft8decoder.processingEnabled;
            config.release(true);
            if (!_this->ft8decoder.processingEnabled) {
                _this->clearDecodedResults(_this->ft8decoder.getModeDM());
            }
        }
        ImGui::SameLine();
        ImGui::Text("Count: %d(%d) in %d..%d msec", _this->ft8decoder.lastDecodeCount.load(), _this->ft8decoder.totalCallsignsDisplayed.load(), _this->ft8decoder.lastDecodeTime0.load(), _this->ft8decoder.lastDecodeTime.load());
        ImGui::LeftLabel("Decode FT4");
        ImGui::FillWidth();
        if (ImGui::Checkbox(CONCAT("##_processing_enabled_ft4_", _this->name), &_this->ft4decoder.processingEnabled)) {
            config.acquire();
            config.conf[_this->name]["processingEnabledFT4"] = _this->ft4decoder.processingEnabled;
            config.release(true);
            if (!_this->ft4decoder.processingEnabled) {
                _this->clearDecodedResults(_this->ft4decoder.getModeDM());
            }
        }
        ImGui::SameLine();
        ImGui::Text("Count: %d(%d) in %d..%d msec", _this->ft4decoder.lastDecodeCount.load(), _this->ft4decoder.totalCallsignsDisplayed.load(), _this->ft4decoder.lastDecodeTime0.load(), _this->ft4decoder.lastDecodeTime.load());
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
    bool enablePSKReporter = true;
    int layoutWidth = 3;
    int secondsToKeepResults = 120;


    //    dsp::buffer::Reshaper<float> reshape;
    //    dsp::sink::Handler<float> diagHandler;
    //    dsp::multirate::RationalResampler<dsp::stereo_t> resamp;
    //

    std::chrono::time_point<std::chrono::high_resolution_clock> lastUpdated;
};


DecodedResult::DecodedResult(DecodedMode mode, long long int decodeEndTimestamp, long long int frequency, const std::string& shortString, const std::string& detailedString)
    : mode(mode), decodeEndTimestamp(decodeEndTimestamp), frequency(frequency), shortString(shortString), detailedString(detailedString) {

}
long long int FT8DrawableDecodedResult::getDecodeEndTimestamp() {
    return result->decodeEndTimestamp;
}
long long int FT8DrawableDecodedResult::getFrequency() {
    return result->frequency;
}

static int toColor(double coss) {
    if (coss < 0) {
        return 0;
    }
    return (int)(coss * 255);
}

void FT8DrawableDecodedResult::draw(const ImVec2& _origin, ImGuiWindow* window) {

    ImVec2 origin = _origin;
    origin += ImVec2(layoutX, layoutY);

    auto phase = result->intensity * 2 * M_PI;
    auto RR= toColor(cos((phase - M_PI / 4 - M_PI - M_PI / 4) / 2));
    auto GG= toColor(cos((phase - M_PI / 4 - M_PI / 2) / 2));
    auto BB = toColor(cos(phase - M_PI / 4));

    ImGui::PushFont(style::tinyFont);
    auto modeSize = ImGui::CalcTextSize("ft8");
    ImGui::PopFont();
    ImGui::PushFont(style::baseFont);
    const char* str = result->shortString.c_str();
    auto textSize = ImGui::CalcTextSize(str);
    ImGui::PopFont();

    ImU32 white = IM_COL32(255, 255, 255, 255);
    ImU32 black = IM_COL32(0, 0, 0, 255);

    //        const ImRect& drect = ImRect(layoutX, layoutY, layoutX + textSize.x, layoutY + textSize.y);
    ImGui::PushFont(style::tinyFont);
    switch(this->result->mode) {
    case DM_FT4:
        window->DrawList->AddRectFilled(origin, origin + ImVec2(modeSize.x, modeSize.y), IM_COL32(255, 0, 200, 255));
        window->DrawList->AddText(origin, white, "ft4");
        break;
    case DM_FT8:
        window->DrawList->AddRectFilled(origin, origin + ImVec2(modeSize.x, modeSize.y), IM_COL32(255, 0, 0, 255));
        window->DrawList->AddText(origin, white, "ft8");
        break;
    }
    ImGui::PopFont();

    origin.x += modeSize.x;

    auto maxCorner = origin + textSize - ImVec2(0, 2);
    window->DrawList->AddRectFilled(origin, maxCorner, IM_COL32(RR, GG, BB, 160));

    if (ImGui::IsMouseHoveringRect(origin, maxCorner)) {
        char buf[128];
        ImGui::BeginTooltip();

        ImGui::Text("Distance: %0.0f km", result->distance);
        ImGui::Separator();
        ImGui::Text("Strength: %0.0f dB", result->strengthRaw);
        ImGui::Separator();
        ImGui::Text("QTH: %s", result->qth.c_str());
        ImGui::Separator();
        ImGui::Text("Message: %s", result->detailedString.c_str());

        ImGui::EndTooltip();
    }


    ImGui::PushFont(style::baseFont);
    window->DrawList->AddText(origin + ImVec2(-1, -1), black, str);
    window->DrawList->AddText(origin + ImVec2(-1, +1), black, str);
    window->DrawList->AddText(origin + ImVec2(+1, -1), black, str);
    window->DrawList->AddText(origin + ImVec2(+1, +1), black, str);
    window->DrawList->AddText(origin, white, str);
    ImGui::PopFont();
    float nsteps = 12.0;
    float h = 4.0;
    float yy = textSize.y - h;
    for(float x=0; x<baseTextSize.x; x+=baseTextSize.x/nsteps) {
        auto x0 = x;
        auto x1 = x + baseTextSize.x/nsteps;
        auto x0m = x0 + (0.8)*(x1-x0);
        bool green = (x / baseTextSize.x) < result->strength;
        window->DrawList->AddRectFilled(ImVec2(x0, yy) + origin, ImVec2(x0m, yy+h) + origin, green ? IM_COL32(64, 255, 0, 255) : IM_COL32(180, 180, 180, 255));
        window->DrawList->AddRectFilled(ImVec2(x0m, yy) + origin, ImVec2(x1, yy+h) + origin, IM_COL32(180, 180, 180, 255));
    }

}
FT8DrawableDecodedResult::FT8DrawableDecodedResult(DecodedResult* result) : result(result) {
    info = result->shortString.c_str();
}

FT8DrawableDecodedDistRange::FT8DrawableDecodedDistRange(const std::string& extraText, long long freq) : extraText(extraText), freq(freq) {
    info = "range";

}
long long int FT8DrawableDecodedDistRange::getDecodeEndTimestamp() {
    return 0;
}
long long int FT8DrawableDecodedDistRange::getFrequency() {
    return freq;
}
void FT8DrawableDecodedDistRange::draw(const ImVec2& _origin, ImGuiWindow* window) {
    ImVec2 origin = _origin;
    origin += ImVec2(layoutX, layoutY);
    ImGui::PushFont(style::tinyFont);
    const char* str = this->extraText.c_str();
    auto strSize = ImGui::CalcTextSize(str);
    ImU32 white = IM_COL32(255, 255, 255, 255);
    window->DrawList->AddRectFilled(origin, origin + strSize, IM_COL32(0, 0, 0, 255));
    window->DrawList->AddText(origin, white, str);
    ImGui::PopFont();
}

void SingleDecoder::handleData(int rd, dsp::stereo_t* inputData) {
    beforeAdjust -= rd;
    if (beforeAdjust <= 0) {
        beforeAdjust = (int)(VFO_SAMPLE_RATE * ADJUST_PERIOD);
        auto [newOffset, centerOffset] = calculateVFOCenterOffsetForMode(gui::waterfall.getCenterFrequency(), gui::waterfall.getBandwidth());
        if (centerOffset != previousCenterOffset) {
            mod->clearDecodedResults(getModeDM());
            previousCenterOffset = centerOffset;
        }
        if (newOffset == INVALID_OFFSET) {
            if (onTheFrequency) {
                flog::info("ON FREQUENCY: FALSE ({}) {} {} -> {}", getModeString(), gui::waterfall.getCenterFrequency(), gui::waterfall.getBandwidth(), newOffset);
                onTheFrequency = false;
            }
        } else { // not invalid
            if (!onTheFrequency) {
                flog::info("ON FREQUENCY: TRUE ({}) {} {} -> {}", getModeString(), gui::waterfall.getCenterFrequency(), gui::waterfall.getBandwidth(), newOffset);
                onTheFrequency = true;
            }
            if (newOffset == (int)vfoOffset) {
                // do nothing
            }
            else {
                vfoOffset = newOffset;
                flog::info("FT8 vfo ({}): center offset {}, bandwidth: {}", getModeString(), vfoOffset, USB_BANDWIDTH);
                vfo->setOffset(vfoOffset - USB_BANDWIDTH / 2);
            }
        }
    }

    if (mod->enabled && onTheFrequency) {
        reader.resize(rd);
        std::copy(usbDemod->getOutput()->readBuf, usbDemod->getOutput()->readBuf + rd, reader.begin());
        handleIFData(reader);
    }
}
void SingleDecoder::startBlockProcessing(const std::shared_ptr<std::vector<dsp::stereo_t>>& block, int blockNumber, int originalOffset) {
    std::thread processor([=]() {
        SetThreadName(getModeString()+"_startBlockProcessing");
        std::time_t bst = (std::time_t)(blockNumber * getBlockDuration());
        flog::info("Start processing block ({}), size={}, block time: {}", this->getModeString(), block->size(), std::asctime(std::gmtime(&bst)));
        blockProcessorsRunning.fetch_add(1);
        auto started = currentTimeMillis();
        std::unique_ptr<int, std::function<void(int*)>> myPtr(new int, [&](int* p) {
            delete p;
            blockProcessorsRunning.fetch_add(-1);
            flog::info("blockProcessorsRunning released after {} msec", currentTimeMillis() - started);
        });
        int count = 0;
        long long time0 = 0;
        auto handler = [&](int mode, std::vector<std::string> result) {
            if (time0 == 0) {
                time0 = currentTimeMillis();
            }
            auto message = result[4];
            auto pipe = message.find('|');
            std::string callsigns;
            std::string callsign;
            if (pipe != std::string::npos) {
                callsigns = message.substr(pipe + 1);
                message = message.substr(0, pipe);
                std::vector<std::string> callsignsV;
                splitStringV(callsigns, ";", callsignsV);
                if (callsignsV.size() > 1) {
                    callsign = callsignsV[1];
                    callHashCacheMutex.lock();
                    callHashCache.addCall(callsignsV[0], bst * 1000);
                    callHashCache.addCall(callsignsV[1], bst * 1000);
                    callHashCacheMutex.unlock();

                }
                if (!callsign.empty() && callsign[0] == '<') {
                    callHashCacheMutex.lock();
                    auto ncallsign = callHashCache.findCall(callsign, bst * 1000);
                    flog::info("Found call: {} -> {}", callsign, ncallsign);
                    callsign = ncallsign;
                    callHashCacheMutex.unlock();
                }
            } else {
                callsign = extractCallsignFromFT8(message);
            }
            count++;
            if (callsign.empty() || callsign.find('<') != std::string::npos) {  // ignore <..> callsigns
                return;
            }
            double distance = 0;
            CTY::Callsign cs;
            if (callsign.empty()) {
                callsign = "?? " + message;
            } else {
                cs = cty.findCallsign(callsign);
                if (mod->myPos.isValid()) {
                    auto bd = bearingDistance(mod->myPos, cs.ll);
                    distance = bd.distance;
                }
            }
            DecodedResult decodedResult(getModeDM(), (long long)(blockNumber * getBlockDuration() * 1000 + getBlockDuration()), originalOffset, callsign, message);
            decodedResult.distance = (int)distance;
            auto strength = atof(result[1].c_str());
            strength = (strength + 24) / (24 + 24);
            if (strength < 0.0) strength = 0.0;
            if (strength > 1.0) strength = 1.0;
            decodedResult.strength = strength;
            decodedResult.strengthRaw = atof(result[1].c_str());
            decodedResult.intensity = 0;
             // (random() % 100) / 100.0;
            if (!cs.dxccname.empty()) {
                decodedResult.qth = cs.dxccname;
            }
            mod->addDecodedResult(decodedResult);
            return;
        };
        std::thread t0([&]() {
            SetThreadName(getModeString()+"_callDecode");
            auto start = currentTimeMillis();
            dsp::ft8::decodeFT8(getModeString(), VFO_SAMPLE_RATE, block->data(), block->size(), handler);
            auto end = currentTimeMillis();
            flog::info("FT8 decoding ({}) took {} ms", this->getModeString(), end - start);
            lastDecodeCount = (int)count;
            lastDecodeTime = (int)(end - start);
            lastDecodeTime0 = (int)(time0 - start);
        });
        t0.join();

    });
    processor.detach();
}
void SingleDecoder::init(const std::string &name) {
    vfo = new dsp::channel::RxVFO(&iqdata, sigpath::iqFrontEnd.getEffectiveSamplerate(), VFO_SAMPLE_RATE, USB_BANDWIDTH, vfoOffset);

    sigpath::iqFrontEnd.onEffectiveSampleRateChange.bindHandler(&iqSampleRateListener);
    iqSampleRateListener.ctx = this;
    iqSampleRateListener.handler = [](double newSampleRate, void* ctx) {
        flog::info("FT8 decoder: effective sample rate changed to {}", newSampleRate);
        ((SingleDecoder*)ctx)->vfo->setInSamplerate(newSampleRate);
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
        flog::info("ifchain change out");
        // next
    });

    gui::mainWindow.onPlayStateChange.bindHandler(&onPlayStateChange);
    onPlayStateChange.ctx = this;
    onPlayStateChange.handler = [](bool playing, void* ctx) {
        SingleDecoder* thiz = (SingleDecoder*)ctx;
        thiz->mod->clearDecodedResults(thiz->getModeDM());
    };
    running = true;



    std::thread reader([thiz=this]() {
        SetThreadName(thiz->getModeString()+"_ssb_reader");

//        auto _this = this;
        while (thiz->running.load()) {
            int rd = thiz->usbDemod->getOutput()->read();
            if (rd < 0) {
                break;
            }
            if (thiz->mod != thiz->mod2) {
                abort();
            }
            if (thiz->processingEnabled) {
                thiz->handleData(rd, thiz->usbDemod->getOutput()->readBuf);
            }
            thiz->usbDemod->getOutput()->flush();
        }
    });
    reader.detach();
}

void SingleDecoder::destroy() {
    sigpath::iqFrontEnd.onEffectiveSampleRateChange.unbindHandler(&iqSampleRateListener);
    gui::mainWindow.onPlayStateChange.unbindHandler(&onPlayStateChange);
    ifChain.out->stopReader();
    running = false;
}


MOD_EXPORT void _INIT_() {
    // Create default recording directory
    json def = json({});
    config.setPath(core::args["root"].s() + "/ft8_decoder_config.json");
    config.load(def);
    config.enableAutoSave();
    std::string resDir = core::configManager.conf["resourcesDirectory"];
    loadCTY(resDir + "/cty/cty.dat", "", cty);
    loadCTY(resDir + "/cty/AF_cty.dat", ", AF", cty);
    loadCTY(resDir + "/cty/BY_cty.dat", ", CN", cty);
    loadCTY(resDir + "/cty/EU_cty.dat", ", EU", cty);
    loadCTY(resDir + "/cty/NA_cty.dat", ", NA", cty);
    loadCTY(resDir + "/cty/SA_cty.dat", ", SA", cty);
    loadCTY(resDir + "/cty/VK_cty.dat", ", VK", cty);
    loadCTY(resDir + "/cty/cty_rus.dat", ", RUS", cty);

#ifdef __ANDROID__
    auto console_sink = std::make_shared<flog::sinks::android_sink_st>("SDR++");
    auto logger = std::shared_ptr<flog::logger>(new flog::logger("", { console_sink }));
    flog::set_default_logger(logger);
#endif

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
