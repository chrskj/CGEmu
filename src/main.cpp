#include "cg6502.hpp"
#include "Bus.hpp"
#include "common.hpp"
#include <cstdio>
#include <fstream> /*For basic I/O.*/
#include <chrono>
#include <thread>
#include <iostream>


int main()
{
	Bus Nes;

	std::string file = "/home/chrisgsk/projects/CGEmu/myfile.bin";
	load_file(file, Nes.ram);

	print_memory(Nes.ram);

	printf("Hello there!");

	std::string curr_comm;
	while (true)
	{
		Nes.cpu.clock();

		std::this_thread::sleep_for(std::chrono::milliseconds(100) );

		curr_comm = Nes.cpu.lookup[Nes.cpu.opcode].name;
		std::cout << curr_comm << std::endl;
	}
	

	return 0;
}