cmake_minimum_required(VERSION 3.13)
project(noise_reduction_logmmse)

file(GLOB SRC "src/*.cpp")

include(${SDRPP_MODULE_CMAKE})

target_include_directories(noise_reduction_logmmse PRIVATE "src/")
target_include_directories(noise_reduction_logmmse PRIVATE "../../decoder_modules/radio/src")
