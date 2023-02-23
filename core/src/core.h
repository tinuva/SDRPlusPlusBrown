#pragma once
#include <config.h>
#include <module.h>
#include <module_com.h>
#include "command_args.h"

namespace core {
    SDRPP_EXPORT ConfigManager configManager;
    SDRPP_EXPORT ModuleManager moduleManager;
    SDRPP_EXPORT ModuleComManager modComManager;
    SDRPP_EXPORT CommandArgsParser args;

    void setInputSampleRate(double samplerate);

    constexpr int NARGUMENTS = 20000;
    struct SpawnCommand {
        char executable[500];
        char args[10][500];
        int nargs;
        char outPath[500];
        char errPath[500];
        std::atomic<bool> completed = false;
        int pid;
        int seq;
    };

    SDRPP_EXPORT bool forkIt(SpawnCommand &cmd);

};

int sdrpp_main(int argc, char* argv[]);