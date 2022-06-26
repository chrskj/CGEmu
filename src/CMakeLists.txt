include_directories(${CGEmu_SOURCE_DIR}/src)

set(SOURCES main.cpp
            Bus.cpp
            cg6502.cpp
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

