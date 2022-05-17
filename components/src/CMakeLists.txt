# Notice name prefix of this variable, set by CMake according
# to value given with "project()" in the root CMakeLists.txt.
include_directories(${CGEmu_SOURCE_DIR}/components/inc)
add_library(components test.cpp)