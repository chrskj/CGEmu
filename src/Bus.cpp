#include "Bus.hpp"

Bus::Bus()
{
	for (auto &i : ram) i = 0x00;

	cpu.connect_to_bus(this);
}

Bus::~Bus()
{
}

void Bus::write(uint16_t addr, uint8_t data)
{
   ram[addr] = data;
}

uint8_t Bus::read(uint16_t addr, uint8_t data)
{
   return ram[addr];
}
