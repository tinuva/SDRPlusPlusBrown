# SDR++ (Brown fork), The bloat-free SDR software

[Changelog](changelog.md)

Please see [upstream project page](https://github.com/AlexandreRouma/SDRPlusPlus) for the basic list of its features.

Last merge: 2024-01-23

Please see [brown fork page](https://sdrpp-brown.san.systems) for list of fork features.

## Thanks / Credits

Thanks and due respect to:
 
* original author, Alexandre Rouma, for his great [work](https://github.com/AlexandreRouma/SDRPlusPlus). 
* MSHV author, LZ2HV, for his great [work](http://lz2hv.org/mshv).
* logmmse/python authors for their great [work](https://github.com/wilsonchingg/logmmse).
* OMLSA authors for their great [idea](https://github.com/yuzhouhe2000/OMLSA-IMCRA) and [implementation](https://github.com/xiaochunxin/OMLSA-MCRA).
* imgui-notify author for his great [work](https://github.com/patrickcjk/imgui-notify)

## Feedback

Have an issue? Works worse than original? File an [issue](https://github.com/sannysanoff/SDRPlusPlusBrown/issues).

## Debugging reminders

* to debug in windows in virtualbox env, download mesa opengl32.dll from https://downloads.fdossena.com/Projects/Mesa3D/Builds/MesaForWindows-x64-20.1.8.7z
* make sure you put rtaudiod.dll in the build folder's root otherwise audio sink will not load.
* use system monitor to debug missing dlls while they fail to load.

## Local Android build:

* put into your ~/.gradle/gradle.properties this line: sdrKitRoot=/home/user/SDRPlusPlus/android-sdr-kit/root
  it can obtained from: https://github.com/AlexandreRouma/android-sdr-kit
  
Good luck.

