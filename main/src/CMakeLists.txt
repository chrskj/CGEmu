include_directories(${CGEmu_SOURCE_DIR}/main/inc)
# I assume you want to use LibProject as a library in MainProject.
include_directories(${CGEmu_SOURCE_DIR}/components/inc)
link_directories(${CGEmu_SOURCE_DIR}/components/src)
add_executable(main main.cpp)
target_link_libraries(main components)