project(CGEmu)

# include project source
include_directories(${PROJECT_SOURCE_DIR}/../src)
include_directories(${PROJECT_SOURCE_DIR}/../src/cg6502)
# include mocks only for test code
# include_directories(${PROJECT_SOURCE_DIR}/../mocks)
# include CppUTest headers
include_directories($ENV{CPPUTEST_HOME}/include)

# add cpputest library
add_library(imp_cpputest STATIC IMPORTED)
set_property(TARGET imp_cpputest PROPERTY
             IMPORTED_LOCATION $ENV{CPPUTEST_HOME}/lib/libCppUTest.a)

# build test library for LedDriver
add_library(cg6502Test ./cg6502Test.cpp)

add_executable(RunAllTests RunAllTests.cpp)
# target_link_libraries(RunAllTests imp_cpputest cg6502Test LedDriver RuntimeErrorStub)
target_link_libraries(RunAllTests imp_cpputest cg6502Test cg6502)
