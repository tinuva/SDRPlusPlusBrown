# SDR++ (sannysanoff fork), The bloat-free SDR software<br>

Please see [upstream project page](https://github.com/AlexandreRouma/SDRPlusPlus) for the main list of its features.

This fork has several goals:

* to add Hermes Lite 2 support
* to experiment with various NOISE REDUCTION algorithms 
* to maintain compatibility with upstream project (regular merges from upstream)
* to add various features that are not in the upstream and have hard time being merged there due to various reasons.

## Installing the fork

Note: Fork may or may not work alongside the original project at present moment. In any case, try. On Windows, should be simpler to run it alongside the original project.

go to the  [recent builds page](https://github.com/sannysanoff/SDRPlusPlus/actions/workflows/build_all.yml), find topmost green build,
click on it, then go to the bottom of the page and find artifact for your system (Linux, Windows, MacOS). It looks like this:
![Example of artifact](https://i.imgur.com/iq8t0Fa.png)

Please note you must be logged in to GitHub to be able to download the artifacts.

## Features compared to original project:


* Hermes Lite 2 support (receive only)
* Noise reduction to benefit SSB/AM - wideband and audio frequency. Wideband is visible on the waterfall. Can turn on both. ***Logmmse*** algorithm is used.
* Mouse wheel scrolling of sliders
* Unicode support in fonts, filenames and installation path (UTF-8), on Windows, too.
* Saving of zoom parameter between sessions
* SNR meter charted below SNR meter - good for comparing antennas
* noise floor calculation differs from original.
* simultaneous multiple audio devices support
* ability to output sound on left or right channel only for particular audio device.
* Airspy HF+ Discovery - narrowing of baseband to hide attenuated parts.
* waterfall drawing is greatly optimized for CPU and GPU bandwidth, important for 4K monitors. 
* small screen option - for android landscape, makes screen layout more accessible. 

## Version log (oldest first)


2022.02.19 initial release. 

* Audio AGC controllable via slider (note: default/original is aggressive) - later original author reimplemented it in a different way 
* Interface scaling - later original author reimplemented it in a different way
* Batch of things from features list (above)

2022.02.20

* for Airspy HF+ Discovery, added Fill-In option which cuts off attenuated far sides of the spectrum.
* added secondary audio stream, so you can output same radio to multiple audio cards / virtual cables
* added single-channel (left/right) option for output audio

2022.06.05

* merging from upstream. Project is still on hold because war in my village (grid locator KO80ce).

2022.09.06

* by this time, merge of upstream has been already completed. The log reducion has been greatly optimized compared to previous version.
* you need to enable plugins to use most of fork functionality: noise_reduction_logmmse and hl2_source
* improved waterfall performance

## Feedback

``Have an issue? Works worse than original? File an [issue](https://github.com/sannysanoff/SDRPlusPlus/issues).

Good luck.

## Thanks

<<<<<<< HEAD
Thanks and due respect to original author. 
=======
The preferred IDE is [VS Code](https://code.visualstudio.com/) in order to have similar development experience across platforms and to build with CMake using the command line.

## Install dependencies

* [cmake](https://cmake.org)
* [vcpkg](https://vcpkg.io)
* [PothosSDR](https://github.com/pothosware/PothosSDR) (This will install libraries for most SDRs)
* [RtAudio](https://www.music.mcgill.ca/~gary/rtaudio/) (You have to build and install it in `C:/Program Files (x86)/RtAudio/`)

After this, install the following dependencies using vcpkg:

* fftw3
* glfw3
* zstd

You are probably going to build in 64 bit so make sure vcpkg installs the correct versions using `.\vcpkg.exe install <package>:x64-windows`

## Building using the command line

**IMPORTANT:** Replace `<vcpkg install directory>` with vcpkg's install directory.

```
mkdir build
cd build
cmake .. "-DCMAKE_TOOLCHAIN_FILE=<vcpkg install directory>/scripts/buildsystems/vcpkg.cmake" -G "Visual Studio 16 2019"
cmake --build . --config Release
```

## Running for development

### Create a new configuration root directory

```bat
./create_root.bat
```

This will create the `root_dev` directory that will be used to save the configs of sdrpp and the modules.

You will next need to edit the `root_dev/config.json` file to point to the modules that were built. If the file is missing in your folder run the application once and it will create one with default value -- see later on how to run the application.

### Run SDR++ from the command line

From the top directory, you can simply run:

```bat
./build/Release/sdrpp.exe -r root_dev -s
```

Or, if you wish to run from the build directory e.g. `build/Release` and adapt the relative path to the `root_dev` folder:

```bat
./sdrpp.exe -r ../../root_dev -s
```

The optional `-s` argument is for keeping the console active in order to see the error messages.

Because all the paths are relative, for the rest of the command line instructions we are going to assume you are running from the top directory using the former command.
As mentioned previously you need to edit `root_dev/config.json` to add the modules that were built. From the default configuration file you need to add the paths in the `modules` section. Add to this list all the modules you wish to use.

```json
...
"modules": [
    "./build/radio/Release/radio.dll",
    "./build/recorder/Release/recorder.dll",
    "./build/rtl_tcp_source/Release/rtl_tcp_source.dll",
    "./build/soapy_source/Release/soapy_source.dll",
    "./build/audio_sink/Release/audio_sink.dll"
]
...
```

You also need to change the location of the resource and module directories, for development, I recommend:

```json
...
"modulesDirectory": "root_dev/modules",
...
"resourcesDirectory": "root_dev/res",
...
```

Remember that these paths will be relative to the run directory.

## Installing SDR++

If you choose to run SDR++ for development, you do not need this step.
First, copy over the exe and DLLs from `build/Release/` to `root_dev`.

Next you need to copy over all the modules that were compiled. To do so, copy the DLL file of the module (located in its build folder given below) to the `root_dev/modules` directory and other DLLs (that do not have the exact name of the module) to the `root_dev` directory.

The modules built will be some of the following (Repeat the instructions above for all you wish to use):

* `build/radio/Release/`
* `build/recorder/Release/`
* `build/rtl_tcp_source/Release/`
* `build/spyserver_source/Release/`
* `build/soapy_source/Release/`
* `build/airspyhf_source/Release/`
* `build/plutosdr_source/Release/`
* `build/audio_sink/Release/`

# Building on Linux / BSD

## Select which modules you wish to build

Depending on which module you want to build, you will need to install some additional dependencies.
Here are listed every module that requires addition dependencies. If a module enabled by default and you do not wish to install a particular dependency (or can't, eg. the BladeRF module on Debian Buster),
you can disable it using the module parameter listed in the table below

* soapy_source: SoapySDR + drivers for each SDRs (see SoapySDR docs)
* airspyhf_source: libairspyhf
* plutosdr_source: libiio, libad9361
* audio_sink: librtaudio-dev

## Install dependencies

* cmake
* fftw3
* glfw
* libvolk
* zstd

Next install dependencies based on the modules you wish to build (See previous step)

Note: make sure you're using GCC 8 or later as older versions do not have `std::filesystem` built-in.

## Building

replace `<N>` with the number of threads you wish to use to build

```sh
mkdir build
cd build
cmake ..
make -j<N>
```

## Create a new root directory

```sh
sh ./create_root.sh
```

## Running for development

If you wish to install SDR++, skip to the next step

First run SDR++ from the build directory to generate a default config file

```
./sdrpp -r ../root_dev/
```

Then, you will need to edit the `root_dev/config.json` file to point to the modules that were built. Here is an example of what it should look like:

```json
...
"modules": [
    "./build/radio/radio.so",
    "./build/recorder/recorder.so",
    "./build/rtl_tcp_source/rtl_tcp_source.so",
    "./build/soapy_source/soapy_source.so",
    "./build/audio_sink/audio_sink.so"
]
...
```

Note: You can generate this list automatically by running `find . | grep '\.so' | sed 's/^/"/' | sed 's/$/",/' | sed '/sdrpp_core.so/d'` in the build directory.

You also need to change the location of the resource and module directories, for development, I recommend:

```json
...
"modulesDirectory": "./root_dev/modules",
...
"resourcesDirectory": "./root_dev/res",
...
```

Remember that these paths will be relative to the run directory.

Of course, remember to add entries for all modules that were built and that you wish to use.

Next, from the top directory, you can simply run:

```
./build/sdrpp -r root_dev
```

Or, if you wish to run from the build directory, you will need to correct the directories in the config.json file, and then run:

```
./sdrpp -r ../root_dev
```

## Installing SDR++

To install SDR++, run the following command in your ``build`` folder:

```sh
sudo make install
```

# Building on MacOS

Warning: This is not for the faint of heart and the instructions are mostly untested. It is recommended to use the [nightly builds](https://www.sdrpp.org/nightly) instead.

## Install dependencies

The dependencies are exactly the same as for linux, see that section for the core dependencies as well as the module list for the per-module dependencies.
You will need to install the dependencies using Homebrew.

Make sure to install portaudio as it'll be needed later.

An example install command would be:

```sh
brew install libusb fftw glfw airspy airspyhf portaudio hackrf rtl-sdr libbladerf codec2 && pip3 install mako zstd
```

### Install volk

You will need to install volk from source. Follow the instructions on their repository. On M1 there are a few more manipulations needed.

## Build

You will need a few special cmake argument on top of the linux ones. You will need to enable the portaudio sink modules `-DOPT_BUILD_PORTAUDIO_SINK=ON -DOPT_BUILD_NEW_PORTAUDIO_SINK=ON` and disable the usual rtaudio sink `-DOPT_BUILD_AUDIO_SINK=OFF` as well as the option to tell SDR++ that it will run as a MacOS bundle `-DUSE_BUNDLE_DEFAULTS=ON`. On MacOS versions older than Catalina (10.15), you will also need to use the internal std::filesystem as the OS can't provide it `-DOPT_OVERRIDE_STD_FILESYSTEM=ON`.

Here is an example of build commands that will build almost all modules at the time of writing. You can always check the CI scripts for the latest arguments just in case but this should work. From the top of the SDRPlusPlus directory:

```sh
mkdir build
cd build
cmake .. -DOPT_BUILD_SOAPY_SOURCE=OFF -DOPT_BUILD_BLADERF_SOURCE=ON -DOPT_BUILD_AUDIO_SINK=OFF -DOPT_BUILD_PORTAUDIO_SINK=ON -DOPT_BUILD_NEW_PORTAUDIO_SINK=ON -DOPT_BUILD_M17_DECODER=ON -DUSE_BUNDLE_DEFAULTS=ON -DCMAKE_BUILD_TYPE=Release
make -j<N>
```

## Create bundle and install

From the top of the SDRPlusPlus directory:

```sh
sh make_macos_bundle.sh ./build ./SDR++.app
```

This will create a `SDR++.app` bundle that you can instal like any other MacOS app by dragging it into Applications.

# Module List

Not all modules are built by default. I decided to disable the build of those with large libraries, libraries that can't be installed through the package manager (or pothos) and those that are still in beta.
Modules in beta are still included in releases for the most part but not enabled in SDR++ (need to be instantiated).

## Sources

| Name                | Stage      | Dependencies      | Option                     | Built by default| Built in Release        | Enabled in SDR++ by default |
|---------------------|------------|-------------------|----------------------------|:---------------:|:-----------------------:|:---------------------------:|
| airspy_source       | Working    | libairspy         | OPT_BUILD_AIRSPY_SOURCE    | ✅              | ✅                     | ✅                         |
| airspyhf_source     | Working    | libairspyhf       | OPT_BUILD_AIRSPYHF_SOURCE  | ✅              | ✅                     | ✅                         |
| bladerf_source      | Working    | libbladeRF        | OPT_BUILD_BLADERF_SOURCE   | ⛔              | ⚠️ (not Debian Buster) | ✅                         |
| file_source         | Working    | -                 | OPT_BUILD_FILE_SOURCE      | ✅              | ✅                     | ✅                         |
| hackrf_source       | Working    | libhackrf         | OPT_BUILD_HACKRF_SOURCE    | ✅              | ✅                     | ✅                         |
| hermes_source       | Unfinished | -                 | OPT_BUILD_HERMES_SOURCE    | ✅              | ✅                     | ✅                         |
| limesdr_source      | Working    | liblimesuite      | OPT_BUILD_LIMESDR_SOURCE   | ⛔              | ✅                     | ✅                         |
| plutosdr_source     | Working    | libiio, libad9361 | OPT_BUILD_PLUTOSDR_SOURCE  | ✅              | ✅                     | ✅                         |
| rfspace_source      | Working    | -                 | OPT_BUILD_RFSPACE_SOURCE   | ✅              | ✅                     | ✅                         |
| rtl_sdr_source      | Working    | librtlsdr         | OPT_BUILD_RTL_SDR_SOURCE   | ✅              | ✅                     | ✅                         |
| rtl_tcp_source      | Working    | -                 | OPT_BUILD_RTL_TCP_SOURCE   | ✅              | ✅                     | ✅                         |
| sdrplay_source      | Working    | SDRplay API       | OPT_BUILD_SDRPLAY_SOURCE   | ⛔              | ✅                     | ✅                         |
| sdrpp_server_source | Working    | -                 | OPT_BUILD_SPYSERVER_SOURCE | ✅              | ✅                     | ✅                         |
| soapy_source        | Working    | soapysdr          | OPT_BUILD_SOAPY_SOURCE     | ✅              | ✅                     | ✅                         |
| spyserver_source    | Working    | -                 | OPT_BUILD_SPYSERVER_SOURCE | ✅              | ✅                     | ✅                         |

## Sinks

| Name               | Stage      | Dependencies | Option                       | Built by default| Built in Release | Enabled in SDR++ by default |
|--------------------|------------|--------------|------------------------------|:---------------:|:----------------:|:---------------------------:|
| android_audio_sink | Working    | -            | OPT_BUILD_ANDROID_AUDIO_SINK | ⛔              | ✅              | ⛔                         |
| audio_sink         | Working    | rtaudio      | OPT_BUILD_AUDIO_SINK         | ✅              | ✅              | ✅                         |
| network_sink       | Working    | -            | OPT_BUILD_NETWORK_SINK       | ✅              | ✅              | ✅                         |
| new_portaudio_sink | Beta       | portaudio    | OPT_BUILD_NEW_PORTAUDIO_SINK | ⛔              | ✅              | ⛔                         |
| portaudio_sink     | Beta       | portaudio    | OPT_BUILD_PORTAUDIO_SINK     | ⛔              | ✅              | ⛔                         |

## Decoders

| Name                | Stage      | Dependencies | Option                        | Built by default| Built in Release | Enabled in SDR++ by default |
|---------------------|------------|--------------|-------------------------------|:---------------:|:----------------:|:---------------------------:|
| dmr_decoder         | Unfinished | -            | OPT_BUILD_DMR_DECODER         | ⛔              | ⛔              | ⛔                         |
| falcon9_decoder     | Unfinished | ffplay       | OPT_BUILD_FALCON9_DECODER     | ⛔              | ⛔              | ⛔                         |
| kgsstv_decoder      | Unfinished | -            | OPT_BUILD_KGSSTV_DECODER      | ⛔              | ⛔              | ⛔                         |
| m17_decoder         | Beta       | -            | OPT_BUILD_M17_DECODER         | ⛔              | ✅              | ⛔                         |
| meteor_demodulator  | Working    | -            | OPT_BUILD_METEOR_DEMODULATOR  | ✅              | ✅              | ⛔                         |
| radio               | Working    | -            | OPT_BUILD_RADIO               | ✅              | ✅              | ✅                         |
| weather_sat_decoder | Unfinished | -            | OPT_BUILD_WEATHER_SAT_DECODER | ⛔              | ⛔              | ⛔                         |

## Misc

| Name                | Stage      | Dependencies | Option                      | Built by default | Built in Release | Enabled in SDR++ by default |
|---------------------|------------|--------------|-----------------------------|:----------------:|:----------------:|:---------------------------:|
| discord_integration | Working    | -            | OPT_BUILD_DISCORD_PRESENCE  | ✅              | ✅               | ⛔                         |
| frequency_manager   | Working    | -            | OPT_BUILD_FREQUENCY_MANAGER | ✅              | ✅               | ✅                         |
| recorder            | Working    | -            | OPT_BUILD_RECORDER          | ✅              | ✅               | ✅                         |
| rigctl_client       | Unfinished | -            | OPT_BUILD_RIGCTL_CLIENT     | ⛔              | ⛔               | ⛔                         |
| rigctl_server       | Working    | -            | OPT_BUILD_RIGCTL_SERVER     | ✅              | ✅               | ✅                         |
| scanner             | Beta       | -            | OPT_BUILD_SCANNER           | ✅              | ✅               | ✅                         |
| scheduler           | Unfinished | -            | OPT_BUILD_SCHEDULER         | ⛔              | ⛔               | ⛔                         |

# Troubleshooting

First, please make sure you're running the latest automated build. If your issue is linked to a bug it is likely that is has already been fixed in later releases

## SDR++ crashes then it won't start again no matter what

This is a bug in 1.0.0 that was fixed in 1.0.1

In some cases, if a crash happened while the config was being saved, the config file would be corrupted and SDR++ would refuse to start because of it.

This has now been fixed. If a config file is corrupted it'll just reset it to its default state.

## "hash collision" error when starting

You likely installed the `soapysdr-module-all` package on Ubuntu/Debian. If not it's still a SoapySDR bug caused by multiple soapy modules coming in conflict. Uninstall anything related to SoapySDR then install soapysdr itself and only the soapy modules you actually need.

## "I don't see -insert module name here-, what's going on?"

If the module was included in a later update, it's not enabled in the config. The easiest way to fix this is just to delete the `config.json` file and let SDR++ recreate it (you will lose your setting relating to the main UI like VFO colors, zoom level and theme).
The best option however is to edit the config file to add an instance of the module you wish to have enabled (see the Module List).

## SDR++ crashes when stopping a RTL-SDR

This is a bug recently introduced by libusb1.4
To solve, this, simply downgrade to libusb1.3

## SDR++ crashes when starting a HackRF

If you also have the SoapySDR module loaded (not necessarily enabled), this is a bug in libhackrf. It's caused by libhackrf not checking if it's already initialized.
The solution until a fixed libhackrf version is released is to completely remove the soapy_source module from SDR++. To do this, delete `modules/soapy_source.dll` on windows
or `/usr/lib/sdrpp/plugins/soapy_source.so` on linux.

## Issue not listed here?

If you still have an issue, please open an issue about it or ask on the discord. I'll try to respond as quickly as I can. Please avoid trying to contact me on every platform imaginable thinking I'll respond faster though...

# Contributing

Feel free to submit pull requests and report bugs via the GitHub issue tracker.
I will soon publish a contributing.md listing the code style to use.

# Credits

## Patrons

* Bob Logan
* [Christian Häusler](https://github.com/corvus-ch)
* Croccydile
* Dale L Puckett (K0HYD)
* [Daniele D'Agnelli](https://linkedin.com/in/dagnelli)
* D. Jones
* [EB3FRN](https://www.eb3frn.net/)
* Eric Johnson
* Ernest Murphy (NH7L)
* Flinger Films
* gringogrigio
* Joe Cupano
* Kezza
* Krys Kamieniecki
* Lee Donaghy
* Lee KD1SQ
* .lozenge. (Hank Hill)
* ON4MU
* [Passion-Radio.com](https://passion-radio.com/)
* Paul Maine
* [Scanner School](https://scannerschool.com/)
* [SignalsEverywhere](https://signalseverywhere.com/)
* Syne Ardwin (WI9SYN)
* [W4IPA](https://twitter.com/W4IPAstroke5)
* [Zipper](github.com/reppiZ)

## Contributors

* [Aang23](https://github.com/Aang23)
* [Alexsey Shestacov](https://github.com/wingrime)
* [Aosync](https://github.com/aosync)
* [Benjamin Kyd](https://github.com/benkyd)
* [Benjamin Vernoux](https://github.com/bvernoux)
* [Cropinghigh](https://github.com/cropinghigh)
* [Fred F4EED](http://f4eed.wordpress.com/)
* [Howard0su](https://github.com/howard0su)
* John Donkersley
* [Joshua Kimsey](https://github.com/JoshuaKimsey)
* [Martin Hauke](https://github.com/mnhauke)
* [Marvin Sinister](https://github.com/marvin-sinister)
* [Maxime Biette](https://github.com/mbiette)
* [Paulo Matias](https://github.com/thotypous)
* [Raov](https://twitter.com/raov_birbtog)
* [Cam K.](https://github.com/Starman0620)
* [Shuyuan Liu](https://github.com/shuyuan-liu)
* [Syne Ardwin (WI9SYN)](https://esaille.me/)
* [Szymon Zakrent](https://github.com/zakrent)
* [Tobias Mädel](https://github.com/Manawyrm)
* Youssef Touil
* [Zimm](https://github.com/invader-zimm)


## Libraries used

* [SoapySDR (PothosWare)](https://github.com/pothosware/SoapySDR)
* [Dear ImGui (ocornut)](https://github.com/ocornut/imgui)
* [spdlog (gabime)](https://github.com/gabime/spdlog)
* [json (nlohmann)](https://github.com/nlohmann/json)
* [rtaudio](http://www.portaudio.com/)
* [Portable File Dialogs](https://github.com/samhocevar/portable-file-dialogs)
>>>>>>> upstream/master
