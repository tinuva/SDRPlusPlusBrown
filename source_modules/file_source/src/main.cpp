#include <imgui.h>
#include <spdlog/spdlog.h>
#include <module.h>
#include <gui/gui.h>
#include <signal_path/signal_path.h>
#include <wavreader.h>
#include <core.h>
#include <gui/widgets/file_select.h>
#include <filesystem>
#include <regex>
#include <gui/tuner.h>
#include <iostream>

#define CONCAT(a, b) ((std::string(a) + b).c_str())

SDRPP_MOD_INFO{
    /* Name:            */ "file_source",
    /* Description:     */ "Wav file source module for SDR++",
    /* Author:          */ "Ryzerth",
    /* Version:         */ 0, 1, 1,
    /* Max instances    */ 1
};

ConfigManager config;

class FileSourceModule : public ModuleManager::Instance {
public:
    FileSourceModule(std::string name) : fileSelect("", { "Wav IQ Files (*.wav)", "*.wav", "All Files", "*" }) {
        this->name = name;

        if (core::args["server"].b()) { return; }

        config.acquire();
        fileSelect.setPath(config.conf["path"], true);
        config.release();

        handler.ctx = this;
        handler.selectHandler = menuSelected;
        handler.deselectHandler = menuDeselected;
        handler.menuHandler = menuHandler;
        handler.startHandler = start;
        handler.stopHandler = stop;
        handler.tuneHandler = tune;
        handler.stream = &stream;
        sigpath::sourceManager.registerSource("File", &handler);
    }

    ~FileSourceModule() {
        stop(this);
        sigpath::sourceManager.unregisterSource("File");
    }

    void postInit() {}

    void enable() {
        enabled = true;
    }

    void disable() {
        enabled = false;
    }

    bool isEnabled() {
        return enabled;
    }

private:
    static void menuSelected(void* ctx) {
        FileSourceModule* _this = (FileSourceModule*)ctx;
        core::setInputSampleRate(_this->sampleRate);
        if (_this->centerFreqSet) {
            tuner::tune(tuner::TUNER_MODE_IQ_ONLY, "", _this->centerFreq);
        }
        sigpath::iqFrontEnd.setBuffering(false);
        gui::waterfall.centerFrequencyLocked = true;
        //gui::freqSelect.minFreq = _this->centerFreq - (_this->sampleRate/2);
        //gui::freqSelect.maxFreq = _this->centerFreq + (_this->sampleRate/2);
        //gui::freqSelect.limitFreq = true;
        spdlog::info("FileSourceModule '{0}': Menu Select!", _this->name);
    }

    static void menuDeselected(void* ctx) {
        FileSourceModule* _this = (FileSourceModule*)ctx;
        sigpath::iqFrontEnd.setBuffering(true);
        //gui::freqSelect.limitFreq = false;
        gui::waterfall.centerFrequencyLocked = false;
        spdlog::info("FileSourceModule '{0}': Menu Deselect!", _this->name);
    }

    static void start(void* ctx) {
        FileSourceModule* _this = (FileSourceModule*)ctx;
        if (_this->running) { return; }
        if (_this->reader == NULL) { return; }
        _this->running = true;
        _this->workerThread = _this->float32Mode ? std::thread(floatWorker, _this) : std::thread(worker, _this);
        spdlog::info("FileSourceModule '{0}': Start!", _this->name);
    }

    static void stop(void* ctx) {
        FileSourceModule* _this = (FileSourceModule*)ctx;
        if (!_this->running) { return; }
        if (_this->reader == NULL) { return; }
        _this->stream.stopWriter();
        _this->workerThread.join();
        _this->stream.clearWriteStop();
        _this->running = false;
        _this->reader->rewind();
        spdlog::info("FileSourceModule '{0}': Stop!", _this->name);
    }

    static void tune(double freq, void* ctx) {
        FileSourceModule* _this = (FileSourceModule*)ctx;
        spdlog::info("FileSourceModule '{0}': Tune: {1}!", _this->name, freq);
    }

    static void menuHandler(void* ctx) {
        FileSourceModule* _this = (FileSourceModule*)ctx;

        if (_this->fileSelect.render("##file_source_" + _this->name)) {
            if (_this->fileSelect.pathIsValid()) {
                if (_this->reader != NULL) {
                    _this->reader->close();
                    delete _this->reader;
                }
                try {
                    _this->reader = new WavReader(_this->fileSelect.path);
                    _this->sampleRate = _this->reader->getSampleRate();
                    core::setInputSampleRate(_this->sampleRate);
                    std::string filename = std::filesystem::path(_this->fileSelect.path).filename().string();
                    double newFrequency = _this->getFrequency(filename);
                    _this->streamStartTime = _this->getStartTime(filename);
                    bool fineTune = gui::waterfall.containsFrequency(newFrequency);
//                    auto prevFrequency = sigpath::vfoManager.getName();
                    _this->centerFreq = newFrequency;
                    _this->centerFreqSet = true;
                    tuner::tune(tuner::TUNER_MODE_IQ_ONLY, "", _this->centerFreq);
                    if (fineTune) {
                        // restore the fine tune. When working with file source and restarting the app, the fine tune is lost
//                        tuner::tune(tuner::TUNER_MODE_NORMAL, "_current", prevFrequency);
                    }
                    //gui::freqSelect.minFreq = _this->centerFreq - (_this->sampleRate/2);
                    //gui::freqSelect.maxFreq = _this->centerFreq + (_this->sampleRate/2);
                    //gui::freqSelect.limitFreq = true;
                }
                catch (std::exception e) {
                    spdlog::error("Error: {0}", e.what());
                }
                config.acquire();
                config.conf["path"] = _this->fileSelect.path;
                config.release(true);
            }
        }

        ImGui::Checkbox("Float32 Mode##_file_source", &_this->float32Mode);
    }

    long long streamStartTime = 0;

    static void worker(void* ctx) {
        FileSourceModule* _this = (FileSourceModule*)ctx;
        double sampleRate = _this->reader->getSampleRate();
        int blockSize = sampleRate / 200.0f;
        int16_t* inBuf = new int16_t[blockSize * 2];

        long long samplesRead = 0;
        sigpath::iqFrontEnd.setCurrentStreamTime(_this->streamStartTime);

        while (true) {
            _this->reader->readSamples(inBuf, blockSize * 2 * sizeof(int16_t));
            volk_16i_s32f_convert_32f((float*)_this->stream.writeBuf, inBuf, 32768.0f, blockSize * 2);
            if (!_this->stream.swap(blockSize)) { break; };

            samplesRead += blockSize;
            if (_this->streamStartTime != 0) {
                long long currentTime = _this->streamStartTime + samplesRead * 1000 / sampleRate;
                sigpath::iqFrontEnd.setCurrentStreamTime(currentTime);
            }
        }

        delete[] inBuf;
    }

    static void floatWorker(void* ctx) {
        FileSourceModule* _this = (FileSourceModule*)ctx;
        double sampleRate = _this->reader->getSampleRate();
        int blockSize = sampleRate / 200.0f;
        dsp::complex_t* inBuf = new dsp::complex_t[blockSize];

        long long samplesRead = 0;
        sigpath::iqFrontEnd.setCurrentStreamTime(_this->streamStartTime);

        while (true) {
            _this->reader->readSamples(_this->stream.writeBuf, blockSize * sizeof(dsp::complex_t));
            if (!_this->stream.swap(blockSize)) { break; };

            samplesRead += blockSize;
            if (_this->streamStartTime != 0) {
                long long currentTime = _this->streamStartTime + samplesRead * 1000 / sampleRate;
                sigpath::iqFrontEnd.setCurrentStreamTime(currentTime);
            }
        }

        delete[] inBuf;
    }

    double getFrequency(std::string filename) {
        std::regex expr("[0-9]+Hz");
        std::smatch matches;
        std::regex_search(filename, matches, expr);
        spdlog::warn("{0} {1}", filename, matches.size());
        if (matches.empty()) { return 0; }
        std::string freqStr = matches[0].str();
        return std::atof(freqStr.substr(0, freqStr.size() - 2).c_str());
    }

    long long getStartTime(std::string filename) {
        // like baseband_14235774Hz_12-19-14_10-07-2022.wav

        if (filename.substr(0, 8) != "baseband") return 0;
        if (".wav" != filename.substr(filename.size() - 4, 4)) return 0;
        auto pos = filename.find("Hz");
        if (pos == std::string::npos) return 0;
        std::string dateTimeStre = filename.substr(pos+3, 19);
        std::tm tm;
        char* end;
#ifdef _WIN32
        int n = sscanf(dateTimeStre.c_str(), "%d-%d-%d_%d-%d-%d", &tm.tm_hour, &tm.tm_min, &tm.tm_sec, &tm.tm_mday, &tm.tm_mon, &tm.tm_year);
        tm.tm_mon--;
        if (n == 6) {
            end = nullptr;
        }
        else {
            end = "X";
        }
#else
        end = strptime(dateTimeStre.c_str(), "%H-%M-%S_%d-%m-%Y", &tm);
#endif
        if (!end || *end == 0) {
            std::time_t t1 = std::mktime(&tm);
            std::cout << std::asctime(&tm) << '\n';
            return ((long long)t1) * 1000;
        } else {
            return 0;
        }
    }

    FileSelect fileSelect;
    std::string name;
    dsp::stream<dsp::complex_t> stream;
    SourceManager::SourceHandler handler;
    WavReader* reader = NULL;
    bool running = false;
    bool enabled = true;
    float sampleRate = 1000000;
    std::thread workerThread;

    double centerFreq = 100000000;
    bool centerFreqSet = false;

    bool float32Mode = false;
};

MOD_EXPORT void _INIT_() {
    json def = json({});
    def["path"] = "";
    config.setPath(core::args["root"].s() + "/file_source_config.json");
    config.load(def);
    config.enableAutoSave();
}

MOD_EXPORT void* _CREATE_INSTANCE_(std::string name) {
    return new FileSourceModule(name);
}

MOD_EXPORT void _DELETE_INSTANCE_(void* instance) {
    delete (FileSourceModule*)instance;
}

MOD_EXPORT void _END_() {
    config.disableAutoSave();
    config.save();
}