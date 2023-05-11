$build_dir=$args[0]
$root_dir=$args[1]

mkdir sdrpp_windows_x64

RELDIR=RelWithDebInfo

# Copy root
cp -Recurse $root_dir/* sdrpp_windows_x64/

# Copy core
cp $build_dir/RelWithDebInfo/* sdrpp_windows_x64/
cp 'C:/Program Files/PothosSDR/bin/volk.dll' sdrpp_windows_x64/

# Copy source modules
cp $build_dir/source_modules/airspy_source/RelWithDebInfo/airspy_source.dll sdrpp_windows_x64/modules/
cp 'C:/Program Files/PothosSDR/bin/airspy.dll' sdrpp_windows_x64/

cp $build_dir/source_modules/airspyhf_source/RelWithDebInfo/airspyhf_source.dll sdrpp_windows_x64/modules/
cp 'C:/Program Files/PothosSDR/bin/airspyhf.dll' sdrpp_windows_x64/

cp $build_dir/source_modules/audio_source/RelWithDebInfo/audio_source.dll sdrpp_windows_x64/modules/

cp $build_dir/source_modules/bladerf_source/RelWithDebInfo/bladerf_source.dll sdrpp_windows_x64/modules/
cp 'C:/Program Files/PothosSDR/bin/bladeRF.dll' sdrpp_windows_x64/

cp $build_dir/source_modules/file_source/RelWithDebInfo/file_source.dll sdrpp_windows_x64/modules/
cp $build_dir/source_modules/hl2_source/RelWithDebInfo/hl2_source.dll sdrpp_windows_x64/modules/

cp $build_dir/source_modules/hackrf_source/RelWithDebInfo/hackrf_source.dll sdrpp_windows_x64/modules/
cp 'C:/Program Files/PothosSDR/bin/hackrf.dll' sdrpp_windows_x64/

cp $build_dir/source_modules/hermes_source/RelWithDebInfo/hermes_source.dll sdrpp_windows_x64/modules/

cp $build_dir/source_modules/limesdr_source/RelWithDebInfo/limesdr_source.dll sdrpp_windows_x64/modules/
cp 'C:/Program Files/PothosSDR/bin/LimeSuite.dll' sdrpp_windows_x64/

cp $build_dir/source_modules/plutosdr_source/RelWithDebInfo/plutosdr_source.dll sdrpp_windows_x64/modules/
cp 'C:/Program Files/PothosSDR/bin/libiio.dll' sdrpp_windows_x64/
cp 'C:/Program Files/PothosSDR/bin/libad9361.dll' sdrpp_windows_x64/

cp $build_dir/source_modules/rfspace_source/RelWithDebInfo/rfspace_source.dll sdrpp_windows_x64/modules/

cp $build_dir/source_modules/rtl_sdr_source/RelWithDebInfo/rtl_sdr_source.dll sdrpp_windows_x64/modules/
cp 'C:/Program Files/PothosSDR/bin/rtlsdr.dll' sdrpp_windows_x64/

cp $build_dir/source_modules/rtl_tcp_source/RelWithDebInfo/rtl_tcp_source.dll sdrpp_windows_x64/modules/

cp $build_dir/source_modules/sdrplay_source/RelWithDebInfo/sdrplay_source.dll sdrpp_windows_x64/modules/ -ErrorAction SilentlyContinue
cp 'C:/Program Files/SDRplay/API/x64/sdrplay_api.dll' sdrpp_windows_x64/ -ErrorAction SilentlyContinue

cp $build_dir/source_modules/sdrpp_server_source/RelWithDebInfo/sdrpp_server_source.dll sdrpp_windows_x64/modules/

cp $build_dir/source_modules/soapy_source/RelWithDebInfo/soapy_source.dll sdrpp_windows_x64/modules/

cp $build_dir/source_modules/spyserver_source/RelWithDebInfo/spyserver_source.dll sdrpp_windows_x64/modules/

cp $build_dir/source_modules/kiwisdr_source/RelWithDebInfo/kiwisdr_source.dll sdrpp_windows_x64/modules/


# Copy sink modules
cp $build_dir/sink_modules/audio_sink/RelWithDebInfo/audio_sink.dll sdrpp_windows_x64/modules/
cp "C:/Program Files (x86)/RtAudio/bin/rtaudio.dll" sdrpp_windows_x64/

cp $build_dir/sink_modules/network_sink/RelWithDebInfo/network_sink.dll sdrpp_windows_x64/modules/


# Copy decoder modules
cp $build_dir/decoder_modules/m17_decoder/RelWithDebInfo/m17_decoder.dll sdrpp_windows_x64/modules/
cp $build_dir/decoder_modules/ft8_decoder/RelWithDebInfo/ft8_decoder.dll sdrpp_windows_x64/modules/
cp $build_dir/decoder_modules/ft8_decoder/RelWithDebInfo/sdrpp_ft8_mshv.exe sdrpp_windows_x64/
cp "C:/Program Files/codec2/lib/libcodec2.dll" sdrpp_windows_x64/

cp $build_dir/decoder_modules/meteor_demodulator/RelWithDebInfo/meteor_demodulator.dll sdrpp_windows_x64/modules/

cp $build_dir/decoder_modules/radio/RelWithDebInfo/radio.dll sdrpp_windows_x64/modules/


# Copy misc modules
cp $build_dir/misc_modules/discord_integration/RelWithDebInfo/discord_integration.dll sdrpp_windows_x64/modules/
cp $build_dir/misc_modules/noise_reduction_logmmse/RelWithDebInfo/noise_reduction_logmmse.dll sdrpp_windows_x64/modules/

cp $build_dir/misc_modules/frequency_manager/RelWithDebInfo/frequency_manager.dll sdrpp_windows_x64/modules/

cp $build_dir/misc_modules/recorder/RelWithDebInfo/recorder.dll sdrpp_windows_x64/modules/

cp $build_dir/misc_modules/rigctl_client/RelWithDebInfo/rigctl_client.dll sdrpp_windows_x64/modules/

cp $build_dir/misc_modules/rigctl_server/RelWithDebInfo/rigctl_server.dll sdrpp_windows_x64/modules/

cp $build_dir/misc_modules/scanner/RelWithDebInfo/scanner.dll sdrpp_windows_x64/modules/


# Copy supporting libs
cp 'C:/Program Files/PothosSDR/bin/libusb-1.0.dll' sdrpp_windows_x64/
cp 'C:/Program Files/PothosSDR/bin/pthreadVC2.dll' sdrpp_windows_x64/

Compress-Archive -Path sdrpp_windows_x64/ -DestinationPath sdrpp_windows_x64.zip

rm -Force -Recurse sdrpp_windows_x64
