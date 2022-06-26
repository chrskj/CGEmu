#pragma once

#include "cg6502.hpp"
#include <array>

class Bus
{
   public:
	Bus();
	~Bus();
	void write(uint16_t addr, uint8_t data);
	uint8_t read(uint16_t addr, uint8_t data);

	// Devices on the bus
	cg6502 cpu;
	std::array<uint8_t, 64 * 1024> ram;

   private:
};
