cmake_minimum_required(VERSION 3.19.6)

set(This Nes)

project(${This} C CXX)

set(CMAKE_C_STANDARD 99)
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_POSITION_INDEPENDENT_CODE ON)

file(GLOB_RECURSE fileCollection "src/*.cpp")

enable_testing()

add_subdirectory(GoogleTest)

add_library(l${This} STATIC ${fileCollection}) 

add_executable(Nes ${fileCollection})

add_subdirectory(test)
