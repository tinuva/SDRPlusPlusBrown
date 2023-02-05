cmake_minimum_required(VERSION 3.13)
project(ft8_decoder)

file(GLOB_RECURSE SRC "src/*.cpp" "src/*.c")

add_library(ft8_decoder SHARED ${SRC})
target_link_libraries(ft8_decoder PRIVATE sdrpp_core)
set_target_properties(ft8_decoder PROPERTIES PREFIX "")

target_include_directories(ft8_decoder PRIVATE "src/")

if (MSVC)
    target_compile_options(ft8_decoder PRIVATE /O2 /Ob2 /std:c++17 /EHsc)
elseif (CMAKE_CXX_COMPILER_ID MATCHES "Clang")
    target_compile_options(ft8_decoder PRIVATE -O3 -std=c++17 -fsanitize=memory -Wno-unused-command-line-argument -undefined dynamic_lookup)
    target_link_options(ft8_decoder PRIVATE -fsanitize=memory)
else ()
    target_compile_options(ft8_decoder PRIVATE -O3 -std=c++17)
endif ()

if (MSVC)
    # Lib path
    target_include_directories(ft8_decoder PRIVATE "C:/Program Files/codec2/include/")
    target_link_directories(ft8_decoder PRIVATE "C:/Program Files/codec2/lib")

    target_link_libraries(ft8_decoder PRIVATE libcodec2)
elseif (ANDROID)
    target_include_directories(ft8_decoder PUBLIC
        /mnt/android_sdr/codec2/src
        /mnt/android_sdr/output/codec2/${ANDROID_ABI}/
    )

    target_link_libraries(ft8_decoder PUBLIC
        /mnt/android_sdr/output/codec2/${ANDROID_ABI}/libcodec2.so
    )
else ()
    find_package(PkgConfig)

    pkg_check_modules(LIBCODEC2 REQUIRED codec2)

    target_include_directories(ft8_decoder PRIVATE ${LIBCODEC2_INCLUDE_DIRS})
    target_link_directories(ft8_decoder PRIVATE ${LIBCODEC2_LIBRARY_DIRS})
    target_link_libraries(ft8_decoder PRIVATE ${LIBCODEC2_LIBRARIES})

    # Include it because for some reason pkgconfig doesn't look here?
    if (${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
        target_include_directories(ft8_decoder PRIVATE "/usr/local/include")
    endif()

endif ()



# Install directives
install(TARGETS ft8_decoder DESTINATION lib/sdrpp/plugins)