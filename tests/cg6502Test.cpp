#include "cg6502.hpp"
#include "Bus.hpp"
#include "common.hpp"
#include "CppUTest/TestHarness.h" // Include after personal includes

TEST_GROUP(TestCpu)
{
};

TEST(TestCpu, testReset)
{
	Bus Nes;
    std::string file = "/home/chrisgsk/projects/CGEmu/files/myfile.bin";
    load_file(file, Nes.ram, 0x0000, 64*1024);
    // test_cpu.reset();
    // test_cpu.
    FAIL("Fail me!");
}