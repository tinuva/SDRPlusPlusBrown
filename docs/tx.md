
# SDR++Brown transmit functionality


## Introduction

### Supported hardware

Supported hardware:

* SDRs with Hermes Lite 2 protocol. I have two different models of this transcievers, 
nothing special. Probably, older models and various modifications will work, too. However, there's nothing to control older model's built-in microphone/speaker. 

If you want your hardware to be added, contact me or make code pull request.

### What works

You can receive/transmit SSB (USB/LSB). You can use your microphone of your desktop/notebook computer. You can run sdr++ on the 
Android phone while laying on the sofa and use microphone of your smartphone, and use wireless connection to the transciever.
Sofa was the primary driving factor for this software development, and I will add everything to make it work better, 
including small logbook for QSO logging etc. Jokes aside, portable use was primary goal.

See example QSO video [https://www.youtube.com/watch?v=CGCXxUXLwr4](https://www.youtube.com/watch?v=CGCXxUXLwr4)

Audio pipeline test [https://www.youtube.com/watch?v=HNrYdeMvFGE](https://www.youtube.com/watch?v=HNrYdeMvFGE)

Video of portable operation here: [https://www.youtube.com/watch?v=wPCf6JJpYos]https://www.youtube.com/watch?v=wPCf6JJpYos this one is with the phone wired to the transciever via USB-C Ethernet, without WiFi, hence lack of operator mobility. 

### What works poorly

* When you use WiFi, you'll have some packet loss, in best conditions it's quite bearable. 
  It may cause the relay clicks, because it causes IQ stream interruption, and transciever responds this way.
* On Android, SSB audio as played on the phone speaker that sucks on most phone models when comparing it to any headphones inserted. However, 
  in portable conditions, this is acceptable. 

### Recommendations

* Use headphones on Android.
* Use wired connection if possible. Some androids support ethernet over USB, and it works well. Buy USB-C dock with ethernet port. It can also charge your phone while you use it. My third option for the phones that do not support usb ethernet, would be RNDIS(over USB) with an addition of intermediate single board computer (idea is reaping).
* If using WiFi, use 5GHz band, because 2.4GHz could be close to useless, depending on the environment. Portable router with 5GHz band is a good idea.
* Try narrow band for receiving (48 khz) if you have packet loss. It may help.

## How to use

### Selection of the source

As of May 2023, Hermes Lite 2 and compatible is used. You must use hl2_source in sdr++. There's hermes_source,
which is intended to receive only, from upstream, i'm not talking about it. 

In the list of drivers, select "Hermes Lite 2". If it's missing, use Module Manager, add module called "hl2_source". 
Select it from dropdown, type name in the empty text field, and press (+)

![](tx_modmanager.jpg)

Select the Hermes Lite 2 in the Source drop down. Press "Refresh" button if fields are empty. The network
broadcast is sent and device is discovered. If your device is discoverable by other software, it will be
discovered by sdr++. If you have your TRX active in other software, stop it there first.

![](tx_source.jpg)

Select bandwidth (start with 48khz). Press play button on the top. You must see the waterfall and hear the noise.
The ADC gain includes both amplification and attenuation in one control. If there's ADC overflow, it
will be drawn in red. As of present, there's no automatic maximum gain now. 

The filters selection below works. On stock HL2, it controls low-pass transmit filters. On my other
model pass-band receive filters. The checkboxes here are not synchronized with the current frequency yet.

### Switching to TRX layout

By default, sdr++brown comes with TRX layout disabled. You can enable it in the settings:

![](tx_layout.jpg)

Press "SSB trx" to switch to the TRX layout. Select "Phone layout" if you prefer some tweaks on Android,
such as wider scrolling sliders and shorter frequency range selector (the artifact of vertical layout ages).

Please not there's experimental audio waterfall. You can enable it in "Show Audio Waterfall" in the very bottom
of the "Display" section (below color map chooser). 

Android smartphone users: You must select smaller scaling factor in the "Display section" so all controls could fit the small 
screen of your phone.

TRX layout differs from receiving layout: 

* Left menu overlays the waterfall. It means, when left menu is active, waterfall is not resized, and while 
  menu is active, touch events are disabled in waterfall. To navigate waterfall, close menu first.
* On the right side there's area with buttons. Buttons control the transmitter.
* On the far right, there are two inertial encoders. Larger one is TUNE knob, at the bottom. Smaller one on top
  is the VOLUME / ZOOM / BRIGHTNESS knob. You are expected to scroll knobs with your fingers. 

![](tx_knobs.jpg)

  
### Search mode

Before you talk, you can search in the frequency band, change bands and do some sound setup. 

![](tx_search_buttons.jpg)

* "Zoom/", "Volume/", "Brightness/" toggles the function of the upper encoder / knob.
* "Audio Cfg" opens audio settings
* "Waterfall" tries to auto-adjust waterfall brightness. Nobody loves it. Use small knob.
* "Mode" allows quick access to bands and modulations. There are some useful defaults like automatic USB/LSB
  selection depending on the frequency. Last frequency is saved per band and modulation using tricky algorithm
  which is not yet perfect and can annoy. You are warned.
* "Zoom" toggles between various predefined zoom modes. Long press shows the full receiving bandwidth.
* "QSO" toggles next mode where you can find TX button

### QSO mode

After you press "QSO" button you get into the transmit mode. Note, if you don't have waterfall
running, you probably won't see the half of the screen, and see red error text instead. 

![](tx_qso.jpg)

Buttons are:

* "Lock" - locks the trequency knob. Because you use your fingers while talking, you sometimes want to
  stay on the frequency.
* "Zoom/", "Volume/", "Brightness/" toggles the function of the upper encoder / knob.
* "Audio Cfg" opens audio settings
* "Soft tune" - generates sine wave. Press again to stop. The hardware tone generation of HL2 is not used. The amplitude of sine wave is controlled by "TX Soft PA". 
* "TX" - transmit button. TX is on while finger is pressing. Microphone sounds are transmitted to the air. Note the hardware "Volume Up" button on Android also works as TX.
* "End QSO" - go back to the Search mode.

On the top you see the oscilloscope of your audio input, as processed.

* "TX IQ" shows the range of IQ stream sent to the radio. Main intention is to control the clipping. 
  Red zone is clipping.  See the [pipeline section](#the-pipeline) for the details.
* "PA enable" to enable 5W power amplifier on the hermes lite 2 board. This is not saved in settings. You 
  must toggle it on manually when you're sober, to not burn up your power transistors.
* SWR, REF PWR show the SWR and reflected power. The forward power is displayed below on the gauge.
* "TX Soft PA" - multiplier for IQ samples sent to the TRX.
* "TX Hard PA" - the 5W power amplifier control provided by the TRX, intended to control the power transistor. 

When you are transmitting the sine wave or the voice, the waterfall will display the spectrum of your voice.
It slightly differs from what can be received over the air, but it mostly reflects the reality. When transmitting without
PA enabled, the signal will be very weak but still visible.

During the transmission, all sounds are muted. The Baseband NR threshold (software) and demodulator AGC state 
are frozen to avoid their unneeded re-adaptation after reception is turned back on. Audio NR2 is not frozen 
(work in progress). 

### The pipeline

The pipeline is represented on the Audio Cfg screen, and the end of the pipeline is "TX Soft PA" described 
above.

![](tx_audio.jpg)

* "Audio lo cutoff frequency" is parameter for high-pass filter that removes bass from the microphone input.
* gauge below represents the signal after that stage.
* "DX equalizer/compressor" is (as of now) the fixed equalizer that boosts the high frequencies and further
  attenuates low frequencies. There's a checkbox. Also, Post-amplifier compensates the compressor results.
* gauge below represents the signal after that stage.
* "Hiss add" - attempts to bring "s" sound in the word "six" from the 5khz into the 1.5khz range of signal. 
  Experimental, may not work in your build.
* "AGC Attack", "AGC Decay" - the signal automatic gain control. Routines are same as on receiving pipeline. 
  I found that setting very low AGC Decay (0.1) helps keeping the signal power pressure mostly constant at 
  maximum, which is good for transmission.
* "Mic NR" - enables the microphone noise reduction (algorithm same as Audio NR2, i.e. OMLSA-MCRA). Very useful on desktop os with simple mic. 
  Not needed for the Android, because Android has its own noise reduction, already used by default.
  Note: the use of processed/raw input in Android pipeline, there's toggle in the Sink section of the main 
  menu. Also note that there's still work in progress, NR2 needs its own AGC that scales from complex numbers to 
  16-bit integers samples because it's implemented in 16-bit integers, and back.
* There's preamp for Mic NR
* gauge below the "Mic NR" represents the signal after that stage.
* "Mic SQL" enables squelch (silence detection) on the microphone. You know when person uses SDR on PC, their signal contains
  a lot of noises in the pauses. In addition to the Mic NR, Mic SQL works very well in producing
  deep silence in the pauses, reflected in emitted radio signal. Pleasure for eyes and ears.
* "High cut-off" - low-pass filter, that coincides with selected mode width (e.g. 2.7 Khz for SSB). You 
  cannot tune it here, it's tuned by the default means in the sdr++, or in the "Mode" dialog.
* Final phase is the amp/attenuator of the final IQ signal that is sent to the transmitter. Use "TX Soft PA"
  slider to control the scale, and observe the TX IQ.
* Clipping occurs in the TX driver and is not shown here. Complex IQ data is translated to 16-bit samples. When
  there's next batch of data to send, if any signal has amplitude more than 1, whole batch is attenuated so 
  that conversion does not overflow during float-to-short conversion. This produces quite nice results, eliminates
  lot of out-of-band emissions and I left it that way.


Buttons "Record" and "Play": ou can record your own voice and replay it back and observe how it sounds after processing. This is only 
for self control. You cannot transmit the recorded voice (yet).  

## Work in progress

This is work in progress. I'm releasing it because it's more useful than not at this stage. 

Also, because this is a new field for me (digital signal processing), my solutions to the problems may be naive, so
advises are welcome. Please join [Telegram group](https://t.me/sdrpp_hermes_fork) with 
questions, bug reports and propositions.


