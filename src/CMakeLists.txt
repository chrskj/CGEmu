project(CGEmu)
include_directories(${CGEmu_SOURCE_DIR})
include_directories(${CGEmu_SOURCE_DIR}/cg6502/)
message(STATUS "Include dir: ${CGEmu_SOURCE_DIR}/cg6502/")

set(SOURCES main.cpp
            Bus.cpp
            common.cpp
            )
if (MSVC)
else()
    link_libraries("-lGL")
    link_libraries("-lX11")
    link_libraries("-lstdc++fs")
    link_libraries("-lpthread")
    link_libraries("-lpng")
endif()

add_executable(main ${SOURCES})
add_library(cg6502 ./cg6502/cg6502.cpp)

target_link_libraries(main cg6502)
