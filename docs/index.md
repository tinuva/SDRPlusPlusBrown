
# SDR++Brown is a fork (patch-set)

* [Download](https://github.com/sannysanoff/SDRPlusPlusBrown/releases/tag/rolling) <--- here, and you may also download [older builds](https://github.com/sannysanoff/SDRPlusPlusBrown/actions/workflows/build_all.yml), however you will need to be logged in to GitHub to download them. 
* [Changelog](https://github.com/sannysanoff/SDRPlusPlusBrown/blob/master/changelog.md)
* [Source code](https://github.com/sannysanoff/SDRPlusPlusBrown/)
* [Читать описание по-русски](https://sdrpp--brown-san-systems.translate.goog/?_x_tr_sch=http&_x_tr_sl=en&_x_tr_tl=ru&_x_tr_hl=en&_x_tr_pto=wapp) через гугл-переводчик, норм переводит, я подстраивался.

Main project is also very good, I am standing on the shoulders of the giants (Alexandre Rouma and others) 
please check it out [by clicking here](http://sdrpp.org). My best recommendations. 

SDR++Brown is maintaining all original features from main project, the synchronization of the source code happens on regular basis. 
Because it is less tested (smaller user base), you can find occasional bugs.

However, this fork adds several features which could not find their way into the main project. 
It has also been observed that addition of some features in this fork improves the chances of same features appearance in the main project!     

Note that different features in this fork have different level of readiness. Click for more details on each feature, or 
read on below.

* [Even more improved rendering performance](#improved-rendering-performance)
* [Bundled FT8 decoder](#bundled-ft8-decoder) - extracted from MSHV code, slightly tweaked.
* [Hermes Lite 2 support](#hermes-lite-2-support) - hl2_source plugin (don't confuse with hermes_source).
* [Transmit mode](#transmit-mode) for Hermes Lite 2 - at the time of the writing, basic SSB transmit is implemented.
* [SNR Chart](#snr-chart) to compare SNR while tweaking antennas, denoising etc.
* Saving of zoom parameters between sessions
* Mouse wheel support on the sliders
* Unicode support in fonts (Cyrillic), filenames and installation path (UTF-8), on Windows, too.
* For Airspy HF+, added Fill-In option which cuts edge sides of the spectrum which are attenuated (low passed) by hardware.
* When replaying WAV file, shows the timestamp, matching the factual time of the recording.
* Experimental, basic KiwiSDR support (12 KHz bandwidth), no UI settings yet.
* [Multiple output audio devices support](#multiple-output-audio-devices-support). Also, possibility to output to the left or right channel only.
* [More display scaling factors](#more-display-scaling-factors). 
* [Bandwidth change slider](#bandwidth-slider) to easily edit bandwidth on touch devices.
* [TCI connector](#tci-connector) (basic), output only. Tested to work with MSHV. 
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
keeping the spirit of Fortran legacy but with C++ syntax, this pure decoder currently supports FT4/FT8 simultaneous decoding. 
You enter your location, and you get decodes instantly, they are grouped by the distance. 
You can even tune to the band elsewhere, it will keep decoding. 
Using this decoder, you can immediately see the band condition and your antenna performance, because FT8-addicted ham operators are always on the air. 
Simultaneous FT4 / FT8 decoding is supported. Decoder is implemented as standalone executable. Detection of secondary streams 
(appearing during DX expedtions) is planned.

![](ft8-decodes.jpg)


## Noise reduction

Noise reduction (logmmse_noise_reduction plugin) works fine for AM/SSB/CW, this is C++ port of 
the [python implementation](https://pypi.org/project/logmmse/). It was made adaptive, so it does not require "silience" training
like original one. 

It is remarkable that SDR++Brown is, af ar as I know, the only place where you can apply noise 
reduction to whole wideband and see it reflected in the waterfall. 
Secondary noise reduction can be applied independently from wideband, it is performed on the audio stream, improving 
(or distorting, whatever you prefer) sound even more. Note that this noise reduction will have small or, more likely, negative effect on the digital modes, 
FM modulated signal etc.

![](noise-reduction.jpg)
![](noise-reduction-toggle.jpg)

## Hermes Lite 2 support

This small device, produced by [Hermes Project](http://www.hermeslite.com/) is an interesting good SDR receiver. 
Made of cheap parts, it has 12bit ADC, 5W Power amplifier, passband filters and 48KHz transmit stream. 
It is connected via Ethernet, so it does not require any drivers.

![](hermes-lite-2.jpg)

Another implementation of same board comes from [LiteSDR project](https://www.litesdr.pp.ua/), formerly Hermes-2000, 
that is slightly less expensive, and has alternative PA and filters schematics, also it is even smaller:

![](hermes-2000.jpg)

## Transmit mode

Transmit mode is currently minimalistic. It supports microphone on desktop OS and built-in microphone on android. 
It has tuning knob with physics/inertia, all what we love.
It has UI (however functional) which is ugly to the degree it cannot be shown here at the moment in its full glory. 
It lacks AGC on TRX and audio level. However, I already made QSO with it in the portable mode. 
There's a picture from video of that historical moment (perspective corrected, fingers are distorted):

![](trx-mode.jpg)

## SNR Chart

This shows the peaks history of SNR meter, so it allows objective comparison of what was before and after you changed 
processing or antenna. The background noise level is calculated slightly differently than in original, showing closer to 0 in the quiet areas.
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
The Volume slider is smaller, the frequency selection is shrunk to sub-GHz range to make more space for the SNR indicator 
(and chart). This is the default option on Android. 

![](small-screen.jpg)

## Bandwidth slider

On touch devices, it is hard to select the bandwidth directly on the waterfall. 
This is why slider has been added on the menu side. Large values outside of slider can 
still be entered manually.

![](bandwidth-slider.jpg)

## TCI connector

This is a basic implementation of TCI connector, which is a protocol for remote control of transceivers. It 
allows uni-directional communication from SDR++ to the transceiver. It supports both audio and wideband
data transfer to the programs. It works for me, it has not been tested by anyone else. Please become a tester.
Demand the TCI support from the software you use!

![](tci-interface.jpg)


## Roadmap

Direction of this fork development points towards:

* Keeping in sync with original project.
* Hearing and decoding more
* Transmitting with ease, with attention to Android, for portable operations.
* More non-canonical and controversial ideas.

