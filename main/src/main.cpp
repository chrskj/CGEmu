#include "cg6502.hpp"
#include "Bus.hpp"
#include <cstdio>

int main()
{
	Bus Nes;
	// Nes.cpu.clock();

	dump_memory(Nes.ram);

	printf("Hello there!");
	return 0;
}