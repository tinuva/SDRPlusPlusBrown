cmake_minimum_required(VERSION 3.13)
project(sdrpp_core)

if (USE_INTERNAL_LIBCORRECT)
    add_subdirectory("libcorrect/")
endif (USE_INTERNAL_LIBCORRECT)

if (USE_BUNDLE_DEFAULTS)
add_definitions(-DIS_MACOS_BUNDLE)
endif (USE_BUNDLE_DEFAULTS)

# Main code
file(GLOB_RECURSE SRC "src/*.cpp" "src/*.c"
        "../misc_modules/noise_reduction_logmmse/src/omlsa_mcra/*.cpp"
        "../misc_modules/noise_reduction_logmmse/src/omlsa_mcra.cpp"
        )
list(FILTER SRC EXCLUDE REGEX "sdrpp_ft8_mshv_main.cpp" )


add_definitions(-DSDRPP_IS_CORE)
add_definitions(-DFLOG_ANDROID_TAG="SDR++")

# Add BUILD_TESTS definition if tests are enabled
if(BUILD_TESTS)
    add_definitions(-DBUILD_TESTS)
    file(GLOB_RECURSE TEST_SRC "../tests/*.cpp")
    set(SRC ${SRC} ${TEST_SRC})
endif()

if(MSVC)
    set(CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS ON)
    add_compile_options(/wd4996)
else()
    add_compile_options(-Wno-deprecated-declarations)
endif()

# Configure backend sources
if (OPT_BACKEND_GLFW)
    file(GLOB_RECURSE BACKEND_SRC "backends/glfw/*.cpp" "backends/glfw/*.c")

endif (OPT_BACKEND_GLFW)
if (OPT_BACKEND_ANDROID)
    file(GLOB_RECURSE BACKEND_SRC "backends/android/*.cpp" "backends/android/*.c")
    set(BACKEND_SRC ${BACKEND_SRC} ${ANDROID_NDK}/sources/android/native_app_glue/android_native_app_glue.c)
endif (OPT_BACKEND_ANDROID)

if(BUILD_TESTS)
    set(SRC ${SRC} ../tests/test_utils.cpp)
endif ()

# Add code to dyn lib
add_library(sdrpp_core SHARED ${SRC} ${BACKEND_SRC})

if(APPLE)
    target_link_libraries(sdrpp_core PUBLIC "-framework CoreFoundation")
    target_link_libraries(sdrpp_core PUBLIC "-framework Accelerate")
    target_link_libraries(sdrpp_core PUBLIC "-framework Security")
    target_link_libraries(sdrpp_core PUBLIC "-framework CFNetwork")
    target_link_libraries(sdrpp_core PUBLIC "-framework AudioToolbox")
endif()


# Set compiler options
target_compile_options(sdrpp_core PRIVATE ${SDRPP_COMPILER_FLAGS})

# Set the install prefix
target_compile_definitions(sdrpp_core PUBLIC INSTALL_PREFIX="${CMAKE_INSTALL_PREFIX}")

# Include core headers
target_include_directories(sdrpp_core PUBLIC "src/")
target_include_directories(sdrpp_core PUBLIC "src/imgui")

# Configure backend includes and libraries
if (OPT_BACKEND_GLFW)
    target_include_directories(sdrpp_core PUBLIC "backends/glfw")
    target_include_directories(sdrpp_core PUBLIC "backends/glfw/imgui")

    if (MSVC)
        # GLFW3
        find_package(glfw3 CONFIG REQUIRED)
        target_link_libraries(sdrpp_core PUBLIC glfw)
    else()
        find_package(PkgConfig)
        pkg_check_modules(GLFW3 REQUIRED glfw3)

        target_include_directories(sdrpp_core PUBLIC ${GLFW3_INCLUDE_DIRS})
        target_link_directories(sdrpp_core PUBLIC ${GLFW3_LIBRARY_DIRS})
        target_link_libraries(sdrpp_core PUBLIC ${GLFW3_LIBRARIES})
    endif()
endif (OPT_BACKEND_GLFW)
if (OPT_BACKEND_ANDROID)
    target_include_directories(sdrpp_core PUBLIC "backends/android")
    target_include_directories(sdrpp_core PUBLIC "backends/android/imgui")
endif (OPT_BACKEND_ANDROID)

# Link to libcorrect
if (USE_INTERNAL_LIBCORRECT)
    target_include_directories(sdrpp_core PUBLIC "libcorrect/include")
    target_link_libraries(sdrpp_core PUBLIC correct_static)
endif (USE_INTERNAL_LIBCORRECT)

if (OPT_OVERRIDE_STD_FILESYSTEM)
    target_include_directories(sdrpp_core PUBLIC "std_replacement")
endif (OPT_OVERRIDE_STD_FILESYSTEM)

if (MSVC)
    # Lib path
    target_link_directories(sdrpp_core PUBLIC "C:/Program Files/PothosSDR/lib/")

    # Misc headers
    target_include_directories(sdrpp_core PUBLIC "C:/Program Files/PothosSDR/include/")

    # Volk
    target_link_libraries(sdrpp_core PUBLIC volk)

    # OpenGL
    find_package(OpenGL REQUIRED)
    target_link_libraries(sdrpp_core PUBLIC OpenGL::GL)

    # GLFW3
    find_package(glfw3 CONFIG REQUIRED)
    target_link_libraries(sdrpp_core PUBLIC glfw)

    # FFTW3
    find_package(FFTW3f CONFIG REQUIRED)
    target_link_libraries(sdrpp_core PUBLIC FFTW3::fftw3f )
    set(FFTW3f_LIBRARIES ${FFTW3_LIBRARIES})
    set(FFTW3f_LIBRARY_DIRS ${FFTW3_LIBRARY_DIRS})
    set(FFTW3f_INCLUDE_DIRS ${FFTW3_INCLUDE_DIRS})
    find_package(FFTW3 CONFIG REQUIRED)
    target_link_libraries(sdrpp_core PUBLIC FFTW3::fftw3 )

    # WinSock2
    target_link_libraries(sdrpp_core PUBLIC wsock32 ws2_32 iphlpapi)

    # ZSTD
    find_package(zstd CONFIG REQUIRED)
    target_link_libraries(sdrpp_core PUBLIC zstd::libzstd_shared)
elseif (ANDROID)
    target_include_directories(sdrpp_core PUBLIC
        ${SDR_KIT_ROOT}/${ANDROID_ABI}/include
        ${ANDROID_NDK}/sources/android/native_app_glue
    )

    target_link_libraries(sdrpp_core PUBLIC
        ${SDR_KIT_ROOT}/${ANDROID_ABI}/lib/libvolk.so
        ${SDR_KIT_ROOT}/${ANDROID_ABI}/lib/libfftw3f.so
        ${SDR_KIT_ROOT}/${ANDROID_ABI}/lib/libzstd.so
        android
        EGL
        GLESv3
        log
    )
else()
    find_package(PkgConfig)
    find_package(OpenGL REQUIRED)



    pkg_check_modules(FFTW3 REQUIRED fftw3f)
    set(FFTW3F_LIBRARIES ${FFTW3_LIBRARIES})
    set(FFTW3F_LIBRARY_DIRS ${FFTW3_LIBRARY_DIRS})
    set(FFTW3F_INCLUDE_DIRS ${FFTW3_INCLUDE_DIRS})
    pkg_check_modules(FFTW3 REQUIRED fftw3)


    # temp

    pkg_check_modules(VOLK REQUIRED volk)
    pkg_check_modules(GLFW3 REQUIRED glfw3)
    pkg_check_modules(LIBZSTD REQUIRED libzstd)

    message(STATUS "VOLK=${VOLK_INCLUDE_DIRS}")


    target_include_directories(sdrpp_core PUBLIC
        ${OPENGL_INCLUDE_DIRS}
        ${FFTW3_INCLUDE_DIRS}
        ${FFTW3F_INCLUDE_DIRS}
        ${GLFW3_INCLUDE_DIRS}
        ${VOLK_INCLUDE_DIRS}
        ${LIBZSTD_INCLUDE_DIRS}
    )

    target_link_directories(sdrpp_core PUBLIC
        ${OPENGL_LIBRARY_DIRS}
        ${FFTW3_LIBRARY_DIRS}
        ${FFTW3F_LIBRARY_DIRS}
        ${GLFW3_LIBRARY_DIRS}
        ${VOLK_LIBRARY_DIRS}
        ${LIBZSTD_LIBRARY_DIRS}
    )

    target_link_libraries(sdrpp_core PUBLIC
        ${OPENGL_LIBRARIES}
        ${FFTW3_LIBRARIES}
        ${FFTW3F_LIBRARIES}
        ${GLFW3_LIBRARIES}
        ${VOLK_LIBRARIES}
        ${LIBZSTD_LIBRARIES}
    )



    if (NOT USE_INTERNAL_LIBCORRECT)
        pkg_check_modules(CORRECT REQUIRED libcorrect)
        target_include_directories(sdrpp_core PUBLIC ${CORRECT_INCLUDE_DIRS})
        target_link_directories(sdrpp_core PUBLIC ${CORRECT_LIBRARY_DIRS})
        target_link_libraries(sdrpp_core PUBLIC ${CORRECT_LIBRARIES})
    endif (NOT USE_INTERNAL_LIBCORRECT)

    if (${CMAKE_SYSTEM_NAME} MATCHES "Linux")
        target_link_libraries(sdrpp_core PUBLIC stdc++fs)
    endif ()

endif ()

set(CORE_FILES ${RUNTIME_OUTPUT_DIRECTORY} PARENT_SCOPE)


# cmake .. "-DCMAKE_TOOLCHAIN_FILE=C:/dev/vcpkg/scripts/buildsystems/vcpkg.cmake"

# Install directives
install(TARGETS sdrpp_core DESTINATION lib)
