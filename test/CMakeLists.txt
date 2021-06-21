cmake_minimum_required(VERSION 3.19.6)

set(This NesTests)

set(Sources NesTests.cpp)

add_executable(${This} ${Sources})

target_link_libraries(${This} PUBLIC gtest_main lNes)

add_test(NAME ${This} COMMAND ${This})

