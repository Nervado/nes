cmake_minimum_required(VERSION 3.19.6)

set(This Nes)

project(${This} C CXX)

set(CMAKE_C_STANDARD 99)
set(CMAKE_CXX_STANDARD 11)
set(CMAKE_POSITION_INDEPENDENT_CODE ON)

file(GLOB_RECURSE fileCollection "src/*.cpp")

enable_testing()

add_subdirectory(GoogleTest)

# set(Headers src/Components/Bus/Bus.h src/Components/Processor/Processor.h)

# set(Sources src/Components/Bus/Bus.cpp src/Components/Processor/Processor.cpp)

# add_library(l${This} STATIC ${Sources} ${Headers}) 

add_library(l${This} STATIC ${fileCollection}) 

#add_executable(${This} ${Sources})

add_executable(Nes ${fileCollection})

add_subdirectory(test)
