#include "cg6502.hpp"
#include "Bus.hpp"
#include "common.hpp"

#define OLC_PGE_APPLICATION
#include "olcPixelGameEngine.h"	 // Only include once

#include <cstdio>
#include <fstream> /*For basic I/O.*/
#include <chrono>
#include <thread>
#include <iostream>
#include <string>

class Example : public olc::PixelGameEngine
{
   public:
	Example()
	{
		sAppName = "Example";
	}

	Bus Nes;
	std::map<uint16_t, std::string> mapAsm;

	std::string hex(uint32_t n, uint8_t d)
	{
		std::string s(d, '0');
		for (int i = d - 1; i >= 0; i--, n >>= 4)
			s[i] = "0123456789ABCDEF"[n & 0xF];
		return s;
	};

	void DrawRam(int x, int y, uint16_t nAddr, int nRows, int nColumns)
	{
		int nRamX = x, nRamY = y;
		for (int row = 0; row < nRows; row++)
		{
			std::string sOffset = "$" + hex(nAddr, 4) + ":";
			for (int col = 0; col < nColumns; col++)
			{
				sOffset += " " + hex(Nes.read(nAddr, true), 2);
				nAddr += 1;
			}
			DrawString(nRamX, nRamY, sOffset);
			nRamY += 10;
		}
	}

	void DrawCpu(int x, int y)
	{
		std::string status = "STATUS: ";
		DrawString(x, y, "STATUS:", olc::WHITE);
		DrawString(x + 64, y, "N", Nes.cpu.P & cg6502::N ? olc::GREEN : olc::RED);
		DrawString(x + 80, y, "V", Nes.cpu.P & cg6502::V ? olc::GREEN : olc::RED);
		DrawString(x + 96, y, "-", Nes.cpu.P & cg6502::U ? olc::GREEN : olc::RED);
		DrawString(x + 112, y, "B", Nes.cpu.P & cg6502::B ? olc::GREEN : olc::RED);
		DrawString(x + 128, y, "D", Nes.cpu.P & cg6502::D ? olc::GREEN : olc::RED);
		DrawString(x + 144, y, "I", Nes.cpu.P & cg6502::I ? olc::GREEN : olc::RED);
		DrawString(x + 160, y, "Z", Nes.cpu.P & cg6502::Z ? olc::GREEN : olc::RED);
		DrawString(x + 178, y, "C", Nes.cpu.P & cg6502::C ? olc::GREEN : olc::RED);
		DrawString(x, y + 10, "PC: $" + hex(Nes.cpu.PC, 4));
		DrawString(x, y + 20, "A: $" + hex(Nes.cpu.A, 2) + "  [" + std::to_string(Nes.cpu.A) + "]");
		DrawString(x, y + 30, "X: $" + hex(Nes.cpu.X, 2) + "  [" + std::to_string(Nes.cpu.X) + "]");
		DrawString(x, y + 40, "Y: $" + hex(Nes.cpu.Y, 2) + "  [" + std::to_string(Nes.cpu.Y) + "]");
		DrawString(x, y + 50, "Stack P: $" + hex(Nes.cpu.S, 4));
	}

	void DrawCode(int x, int y, int nLines)
	{
		auto it_a  = mapAsm.find(Nes.cpu.PC);
		int nLineY = (nLines >> 1) * 10 + y;
		if (it_a != mapAsm.end())
		{
			DrawString(x, nLineY, (*it_a).second, olc::CYAN);
			while (nLineY < (nLines * 10) + y)
			{
				nLineY += 10;
				if (++it_a != mapAsm.end())
				{
					DrawString(x, nLineY, (*it_a).second);
				}
			}
		}

		it_a   = mapAsm.find(Nes.cpu.PC);
		nLineY = (nLines >> 1) * 10 + y;
		if (it_a != mapAsm.end())
		{
			while (nLineY > y)
			{
				nLineY -= 10;
				if (--it_a != mapAsm.end())
				{
					DrawString(x, nLineY, (*it_a).second);
				}
			}
		}
	}

	bool OnUserCreate() override
	{
		// std::string file = "/home/chrisgsk/projects/CGEmu/files/testADC.bin";
		// load_file(file, Nes.ram, 0x8000, 28);
		// Nes.ram[0xFFFC] = 0x00;	 // Set Reset Vector
		// Nes.ram[0xFFFD] = 0x80;

		std::string file = "/home/chrisgsk/projects/CGEmu/files/myfile.bin";
		load_file(file, Nes.ram, 0x0000, 65536);

		// Dont forget to set IRQ and NMI vectors if you want to play with those

		// Extract dissassembly
		mapAsm = Nes.cpu.disassemble(0x0000, 0xFFFF);

		// Reset
		Nes.cpu.reset();
		return true;
	}

	bool OnUserUpdate(float fElapsedTime) override
	{
		Clear(olc::DARK_BLUE);

		if (GetKey(olc::Key::SPACE).bPressed)
		{
			do
			{
				Nes.cpu.clock();
			} while (!Nes.cpu.complete());
		}

		if (GetKey(olc::Key::R).bPressed)
			Nes.cpu.reset();

		// if (GetKey(olc::Key::I).bPressed)
		// 	Nes.cpu.irq();

		// if (GetKey(olc::Key::N).bPressed)
		// 	Nes.cpu.nmi();

		// Draw Ram Page 0x00
		DrawRam(2, 2, 0x0000, 16, 16);
		DrawRam(2, 182, 0x8000, 16, 16);
		DrawCpu(448, 2);
		DrawCode(448, 72, 26);

		DrawString(10, 370, "SPACE = Step Instruction    R = RESET    I = IRQ    N = NMI");

		return true;
	}
};

int main()
{
	Example demo;
	if (demo.Construct(680, 480, 2, 2))
		demo.Start();

	return 0;
}
