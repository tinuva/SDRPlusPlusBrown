# Changelog

Newest entries are at the top.

2023.03.25

* Added parallel (multithreaded) waterfall update. 2 threads give the best result (maybe number memory channels?).
* Added ALL.TXT log output from FT8/FT4 decoder.

2023.03.13

* Added kiwisdr source, no good UI yet. 

2023.03.05

* rewrote ft8 layout, animations. Added bandwidth slider as one user suggested on discord. 
* Added UI scale less than 100% as another user suggested on discord.

2023.03.01

* was playing with slow resizing (zooming) of the waterfall. Found a place to optimize it using SSE vector operations in most deep loop.
  Gained 300% speedup. Still single-threaded, multithreaded will come later.

2023.02.15

* added experimental FT4/FT8 decoding from MSHV code, tweaked for SDR++. All credit goes to MSHV author and down the line.


2023.01.07

* added transmit mode for Hermes Lite 2. Note: using my own driver (hl2_), not the one from upstream.
* added mobile screen mode for android/landscape. With big buttons. Has big ambitions, but in early stage.

2022.09.06

* by this time, merge of upstream has been already completed. The log reducion has been greatly optimized compared to previous version.
* you need to enable plugins to use most of fork functionality: noise_reduction_logmmse and hl2_source
* improved waterfall performance


2022.06.05

* merging from upstream. Project is still on hold because war in my village (grid myGrid KO80ce).



2022.02.20

* for Airspy HF+ Discovery, added Fill-In option which cuts off attenuated far sides of the spectrum.
* added secondary audio stream, so you can output same radio to multiple audio cards / virtual cables
* added single-channel (left/right) option for output audio


2022.02.19 initial release.

* Audio AGC controllable via slider (note: default/original is aggressive) - later original author reimplemented it in a different way
* Interface scaling - later original author reimplemented it in a different way
* Batch of things from features list (above)
