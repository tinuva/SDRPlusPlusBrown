
# SDR++Brown is a fork (patch-set)

* [Download build for your platform here](https://github.com/sannysanoff/SDRPlusPlusBrown/releases/tag/rolling), and you may also download [older builds](https://github.com/sannysanoff/SDRPlusPlusBrown/actions/workflows/build_all.yml), but log in first. 
* [Changelog](https://github.com/sannysanoff/SDRPlusPlusBrown/blob/master/changelog.md)
* [Source code](https://github.com/sannysanoff/SDRPlusPlusBrown/)
* [Читать описание по-русски](https://sdrpp--brown-san-systems.translate.goog/?_x_tr_sch=http&_x_tr_sl=en&_x_tr_tl=ru&_x_tr_hl=en&_x_tr_pto=wapp) через гугл-переводчик, норм переводит, я подстраивался.
* [Telegram channel](https://t.me/sdrpp_hermes_fork) for feedback.

## What is SDR++ (and its fork)

This is the software that allows display and decoding of radio frequency signals on PC and Android. To run the software, you
need the hardware that can receive radio signals, such as [RTL-SDR](https://www.rtl-sdr.com/), [Airspy](https://airspy.com/).
If you're not lucky enough, you can use [KiwiSDR](https://kiwisdr.com/) or [WebSDR](https://websdr.org/) to see how it feels.
(However, you're lucky enough to use KiwiSDR without its web part now, because SDR++Brown begins to support it now.)

## Why fork?

Main project is very good, so I am standing on the shoulders of the giants (Alexandre Rouma and others) 
please check it out [by clicking here](http://sdrpp.org). My best recommendations. 

However, this Brown fork adds several features which could not find their way into the main project. 
It has also been observed that addition of some features in this fork improves the chances of same features appearance in the main project!

SDR++Brown is maintaining all original features from main project, the synchronization of the source code happens on regular basis.
Because it is less tested (smaller user base), you can find occasional bugs.

Note that different features in this fork have different level of readiness. Click for more details on each feature, or 
read on below.

* [Even more improved rendering performance](#improved-rendering-performance)
* [Bundled FT8 decoder](#bundled-ft8-decoder) - extracted from MSHV code, slightly tweaked.
* [Hermes Lite 2 support](#hermes-lite-2-support) - hl2_source plugin (don't confuse with hermes_source).
* [Transmit mode](#transmit-mode) for Hermes Lite 2 - at the time of the writing, basic SSB transmit is implemented. 
* [SNR Chart](#snr-chart) to compare SNR while tweaking antennas, de-noising etc.
* Saving of zoom parameters between sessions
* Mouse wheel support on the sliders
* Unicode support in fonts (Cyrillic), filenames and installation path (UTF-8), on Windows, too.
* For Airspy HF+, added Fill-In option which cuts edge sides of the spectrum which are attenuated (low passed) by hardware.
* When replaying WAV file, shows the timestamp, matching the factual time of the recording.
* Experimental, basic KiwiSDR support (12 KHz bandwidth), no UI settings yet.
* [Multiple output audio devices support](#multiple-output-audio-devices-support). Also, possibility to output to the left or right channel only.
* [More display scaling factors](#more-display-scaling-factors). 
* [Bandwidth change slider](#bandwidth-slider) to easily edit bandwidth on touch devices.
* [TCI protocol](#tci-protocol) (basic), output only. Tested to work with MSHV software. 
* [Roadmap](#roadmap)

## Improved rendering performance

In contrast to current upstream, this fork does not send full image of waterfall to the GPU on each frame, which is important 
for 4K monitors; it burns significantly less CPU by implementing waterfall as tiles and sending 
updates only to the single tile, thus reducing consumed system memory bandwidth.

Also, zooming and re-generation of waterfall is faster by 2-3 times by employing more optimal routines, 
which can be noticed especially on wider SDR bandwidths. Note: upstream author promises to implement scaling in shader, 
which is going to be even better than all this.

## Bundled FT8 decoder

Borrowed from great [MSHV project](http://lz2hv.org/mshv), stripped out of the Qt-specific code, made lightweight, 
keeping the spirit of Fortran legacy but with C++ syntax, this pure decoder currently supports FT4 and FT8 modes. 
You enter your location, and you get decodes grouped by the distance. 
You can even tune on the other frequency, it will keep decoding, while it's within the SDR wideband. 
Using this decoder, you can immediately see the band condition and your antenna performance, because FT8-addicted ham operators are always on the air and fill it with their 
signals. Simultaneous FT4/FT8 decoding is supported. Decoder is implemented as standalone executable. Detection of secondary streams 
(appearing during DX expeditions) is planned.

![](ft8-decodes.jpg)


## Noise reduction

Noise reduction (logmmse_noise_reduction plugin) works fine for AM/SSB/CW, this is C++ port of 
the [python implementation](https://pypi.org/project/logmmse/). It was made adaptive, so it does not require "silence" training
like original one. 

It is remarkable that SDR++Brown is, af ar as I know, the only place where you can apply noise 
reduction to whole wideband and see it reflected in the waterfall. 
Secondary noise reduction can be applied independently of the wideband, it is performed on the audio stream, improving 
(or distorting, whatever you prefer) sound even more. Note that this noise reduction will have small or, more likely, negative effect on the digital modes, 
FM modulated signal etc.

![](noise-reduction.jpg)
![](noise-reduction-toggle.jpg)

## Hermes Lite 2 support

This small device, produced by [Hermes Project](http://www.hermeslite.com/) is an interesting good SDR receiver. 
Made of cheap parts, it has 12bit ADC, 5W Power amplifier, pass-band filters and 48KHz transmit stream bandwidth. 
It is connected via Ethernet, so it does not require any drivers.

![](hermes-lite-2.jpg)

Another implementation of same transceiver comes from [LiteSDR project](https://www.litesdr.pp.ua/), formerly Hermes-2000, 
that is slightly less expensive, and has alternative PA and filters schematics, also it is even smaller:

![](hermes-2000.jpg)

## Transmit mode

Transmit mode is currently minimalistic. It supports microphone on desktop OS and built-in microphone on android. 
It has tuning knob with physics/inertia, all what we love.
It has UI (functional enough) which is ugly to the degree it cannot be shown here at the moment in its full glory. 
It currently lacks AGC on TRX and audio level. However, I already made QSO with it in the portable mode.
Currently, it's in "use on your own risk" mode, because it will be changed a lot.
There's a picture from video of that historical moment (perspective corrected, fingers are distorted):

![](trx-mode.jpg)

## SNR Chart

This shows the peaks history of SNR meter, so it allows objective comparison of what was before and after you changed 
signal processing or switched between your antennas. The background noise level is calculated slightly differently than in original, showing closer to 0 in the quiet areas.
SNR chart is part of logmmse_noise_reduction plugin

![](snr-chart.jpg)
![](snr-toggle.jpg)

## Multiple output audio devices support

You can infinitely configure outputs to the audio devices and other sink devices. On desktop OS, 
you can also configure left or right channel output only. 

![](left-right-channel-audio.jpg)
![](multiple-audio-output.jpg)

## More display scaling factors

You can zoom in and out with bigger choices than in main project. Also, on Android, native scaling factor option is added,
which is detected on startup and brings proper scale factor right from beginning.

![](more-scaling-factors.jpg)

## Small screen support

Even if you're not transmitting on your android phone, but still use SDR++ here, sliders on the right will adapt to the
screen height not to cause overflow / scrollbar. They are wider for finger and re-arranged. 
The Volume slider was made smaller, the frequency selection was shrunk to sub-GHz range to make more space for the SNR indicator 
(and chart). This is the default option on Android. 

![](small-screen.jpg)

## Bandwidth slider

On touch devices, it is hard to select the bandwidth directly on the waterfall. 
This is why slider has been added on the menu side. Large values outside of slider can 
still be entered manually.

![](bandwidth-slider.jpg)

## TCI protocol

This is a basic implementation of TCI connector, which is a protocol for remote control of transceivers. It 
currently allows only uni-directional communication from SDR++ to the transceiver. It supports both audio and wideband
data transfer to the programs. It works for me, but it has not been tested by someone else. Please become a tester.
Demand the TCI support from all other software you use!

![](tci-interface.jpg)


## Roadmap

Direction of this fork development:

* To keep in sync with original project.
* To hear and decode more.
* To Transmit with ease, with attention to Android, for portable operations.
* To add more non-canonical and controversial ideas.

