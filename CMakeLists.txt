cmake_minimum_required(VERSION 3.16.3)
set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

project(CGEmu VERSION 0.1.0)
add_subdirectory(main)
add_subdirectory(components)

# add_executable(${CGEmu_SOURCE_DIR} src/main.cpp)