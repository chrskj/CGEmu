#include "common.hpp"

#include <iostream> /*For basic I/O.*/
#include <fstream>	/*For basic I/O.*/
#include <sstream>	/*For basic I/O.*/
#include <vector>	/*For STL vector.*/
#include <tuple>	/*For packing multi data.*/
#include <algorithm>

void print_memory(const std::array<uint8_t, 64 * 1024> &n)
{
	for (int i = 0; i < (64 * 1024); i++)
	{
		if (i % 16 == 0)
		{
			printf("\n %04X\t", i);
		}
		printf("%02hhX ", n[i]);
	}
	printf("\n");
};

void load_file(std::string file_name, std::array<uint8_t, 64 * 1024> &n, uint16_t _offset, int _size)
{
	try
	{
		/*Open the stream in binary mode.*/
		std::ifstream bin_file(file_name, std::ios::binary);

		if (bin_file.good())
		{
			/*Read Binary data using streambuffer iterators.*/
			std::vector<uint8_t> v_buf((std::istreambuf_iterator<char>(bin_file)), (std::istreambuf_iterator<char>()));
			bin_file.close();

			// std::copy_n(v_buf.begin(), 64 * 1024, n.begin());
			std::copy_n(v_buf.begin(), _size, n.begin() + _offset);
		}
		else
		{
			throw std::exception();
		}
	}

	catch (std::exception &e)
	{
		// std::string ex_str = "Error: " + file_name + ": No such file or directory";
		// throw std::exception(ex_str.c_str());
		std::cerr << "exception caught: " << e.what() << '\n';
		throw std::exception();
	}
}