include_directories(${CGEmu_SOURCE_DIR}/src)

set(SOURCES main.cpp
            Bus.cpp
            cg6502.cpp
            common.cpp
            )
add_executable(main ${SOURCES})
