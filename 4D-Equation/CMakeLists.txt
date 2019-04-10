cmake_minimum_required(VERSION 3.1)

project(4D-Equation)
include_directories(${CMAKE_CURRENT_SOURCE_DIR})

add_library(pso STATIC src/pso.c)
add_executable(core src/main.cpp)
target_link_libraries(core pso)

find_package(OpenMP)
target_link_libraries(core OpenMP::OpenMP_CXX)

find_package(GSL REQUIRED)
target_link_libraries(core GSL::gsl GSL::gslcblas)