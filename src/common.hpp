
#pragma once

#include <array>
#include <string>
#include <vector>
#include <tuple>  /*For packing multi data.*/
#include "stdbool.h"

void print_memory(const std::array<uint8_t, 64 * 1024> &n);
void load_file(std::string file_name, std::array<uint8_t, 64 * 1024> &n, uint16_t _offset, int _size);