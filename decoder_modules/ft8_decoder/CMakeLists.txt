cmake_minimum_required(VERSION 3.13)
project(ft8_decoder)

file(GLOB_RECURSE SRC "src/*.cpp" "src/*.c" ../../misc_modules/recorder/src/wav.cpp ../../misc_modules/recorder/src/riff.cpp)
list(FILTER SRC EXCLUDE REGEX "sdrpp_ft8_mshv.cpp*" )
list(FILTER SRC EXCLUDE REGEX "ft8_etc/*" )

add_library(ft8_decoder SHARED ${SRC})
target_link_libraries(ft8_decoder PRIVATE sdrpp_core)
set_target_properties(ft8_decoder PROPERTIES PREFIX "")

target_include_directories(ft8_decoder PRIVATE "src/")

if (MSVC)
    target_compile_options(ft8_decoder PRIVATE /O2 /Ob2 /std:c++17 /EHsc)
elseif (CMAKE_CXX_COMPILER_ID MATCHES "Clang")
    target_compile_options(ft8_decoder PRIVATE -O3 -std=c++17 -Wno-unused-command-line-argument -undefined dynamic_lookup)
else ()
    target_compile_options(ft8_decoder PRIVATE -O3 -std=c++17)
endif ()

# Install directives
install(TARGETS ft8_decoder DESTINATION lib/sdrpp/plugins)

file(GLOB_RECURSE SDRPP_FT8MSHV_SRC src/*.cpp src/*.c EXCLUDE main.c)
list(FILTER SDRPP_FT8MSHV_SRC EXCLUDE REGEX "main.c*" )

add_executable(sdrpp_ft8_mshv "src/sdrpp_ft8_mshv.cpp" ${SDRPP_FT8MSHV_SRC})
target_include_directories(sdrpp_ft8_mshv PUBLIC "../../core/src")
target_link_libraries(sdrpp_ft8_mshv PUBLIC fftw3 volk)
#target_link_options(test0 PUBLIC -fsanitize=memory)

install(TARGETS sdrpp_ft8_mshv DESTINATION bin)

set(TARGETS sdrpp_ft8_mshv )
foreach(TGT IN LISTS TARGETS)
    if (MSVC)
        target_compile_options(${TGT} PRIVATE /O2 /Ob2 /std:c++17 /EHsc)
    elseif (CMAKE_CXX_COMPILER_ID MATCHES "Clang")
        target_compile_options(${TGT} PRIVATE -std=c++17 -Wno-unused-command-line-argument -undefined dynamic_lookup)
    else ()
        target_compile_options(${TGT} PRIVATE -std=c++17)
    endif ()
endforeach()

