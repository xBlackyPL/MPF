cmake_minimum_required(VERSION 3.1)
project(4D-Equation)

add_library(pso STATIC src/pso.cpp)
add_executable(core src/main.cpp)
target_link_libraries(core pso)

find_package(OpenMP)
target_link_libraries(core OpenMP::OpenMP_CXX)

find_package(GSL REQUIRED)
target_link_libraries(core GSL::gsl GSL::gslcblas)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -O2 -fopenmp" )
