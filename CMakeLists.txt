cmake_minimum_required(VERSION 3.16.3)
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

project(CGEmu VERSION 0.1.0)

# Check for CppUTest and bail out if they don't have it
if(DEFINED ENV{CPPUTEST_HOME})
  message("Using CppUTest found in $ENV{CPPUTEST_HOME}")
else()
  message("CPPUTEST_HOME is not set; You must tell CMake where to find CppUTest")
  return()
endif()


add_subdirectory(src)
# add_subdirectory(mocks)
add_subdirectory(tests)