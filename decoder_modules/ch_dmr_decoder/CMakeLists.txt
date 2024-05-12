cmake_minimum_required(VERSION 3.13)
project(ch_dmr_decoder)


set(DLROOT ${CMAKE_BINARY_DIR})
if (EXISTS ${DLROOT}/mbelib)
    message("Not downloading mbelib")
else ()
    message("Downloading mbelib")
    file(DOWNLOAD https://codeload.github.com/szechyjs/mbelib/tar.gz/9a04ed5c78176a9965f3d43f7aa1b1f5330e771f ${DLROOT}/mbelib.tar.gz)
    execute_process(COMMAND ${CMAKE_COMMAND} -E tar xfz mbelib.tar.gz WORKING_DIRECTORY ${DLROOT})
    FILE(GLOB CONTENT ${DLROOT}/mbelib-*)
    FILE(RENAME ${CONTENT} ${DLROOT}/mbelib)
    FILE(READ "${DLROOT}/mbelib/CMakeLists.txt" TMPF)
    string(REGEX REPLACE "add_custom_target.uninstall" "add_custom_target(mbelib_uninstall" TMPF2 "${TMPF}")
    FILE(WRITE "${DLROOT}/mbelib/CMakeLists.txt" ${TMPF2})
endif ()

# add_custom_target(uninstall

if (EXISTS ${DLROOT}/itpp)
    message("Not downloading itpp")
else ()
    message("Downloading itpp")
    file(DOWNLOAD https://codeload.github.com/mmeidlinger/itpp//tar.gz/d29deaf7560ed84818a82aa2cc6e4b9337ee9017 ${DLROOT}/itpp.tar.gz)
    execute_process(COMMAND ${CMAKE_COMMAND} -E tar xfz itpp.tar.gz WORKING_DIRECTORY ${DLROOT})
    FILE(GLOB CONTENT ${DLROOT}/itpp-*)
    FILE(RENAME ${CONTENT} ${DLROOT}/itpp)

    FILE(READ "${DLROOT}/itpp/CMakeLists.txt" TMPF)
    string(REGEX REPLACE "option.ITPP_SHARED_LIB" "option(ZZZZ_ITPP_SHARED_LIB" TMPF2 "${TMPF}")
    FILE(WRITE "${DLROOT}/itpp/CMakeLists.txt" ${TMPF2})


endif ()
FILE(REMOVE ${DLROOT}/itpp/version)
FILE(REMOVE ${DLROOT}/mbelib/config.h)

set(CMAKE_MODULE_PATH "${DLROOT}/itpp/cmake" ${CMAKE_MODULE_PATH})
add_subdirectory(${DLROOT}/itpp ${DLROOT}/itpp-build)
set(DISABLE_TEST YES)
add_subdirectory(${DLROOT}/mbelib ${DLROOT}/mdelib-build)

file(GLOB_RECURSE SRC "src/*.cpp" "src/*.h" "src/*.hpp" "src/*.c")

include(${SDRPP_MODULE_CMAKE})

if ((CMAKE_BUILD_TYPE STREQUAL Release) OR (NOT CMAKE_BUILD_TYPE))
    set (libitpp_target itpp_static)
else()
    set (libitpp_target itpp_static_debug)
endif()



target_link_libraries(ch_dmr_decoder PRIVATE fftw3 ${libitpp_target} mbe-static)
target_include_directories(ch_dmr_decoder PRIVATE "src/" ${CMAKE_CURRENT_LIST_DIR}/../../core/src/ ${DLROOT}/mbelib ${DLROOT}/itpp ${DLROOT}/ ${DLROOT}/itpp-build)
