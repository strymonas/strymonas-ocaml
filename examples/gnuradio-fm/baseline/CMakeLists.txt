cmake_minimum_required(VERSION 3.8)
set(CMAKE_C_STANDARD 11)
set(CMAKE_CXX_STANDARD 17)

project(gr-fmradio_gnuradio LANGUAGES C CXX)

find_package(Gnuradio "3.10.6.0" COMPONENTS
    blocks
    fft
    filter
    analog
    audio
    soapy
)

add_executable(gr-fmradio_gnuradio utils.c fmradio.cpp main.cpp)
target_link_libraries(gr-fmradio_gnuradio
    gnuradio::gnuradio-blocks
    gnuradio::gnuradio-filter
    gnuradio::gnuradio-analog
    gnuradio::gnuradio-audio
    gnuradio::gnuradio-soapy
)

