project(CGEmu)
include_directories(${CGEmu_SOURCE_DIR})
message(STATUS "Include dir: ${CGEmu_SOURCE_DIR}")

set(SOURCES main.cpp
            cg6502.cpp
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

# target_link_libraries(main cg6502)
