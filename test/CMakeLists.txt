cmake_minimum_required(VERSION 3.19.6)

set(This IntPartTests)

set(Sources IntPartTests.cpp)

add_executable(${This} ${Sources})
target_link_libraries(${This} PUBLIC gtest_main Intpart)

add_test(NAME ${This} COMMAND ${This})

