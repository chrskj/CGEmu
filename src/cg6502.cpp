#include "cg6502.hpp"
#include "Bus.hpp"

#include <map>

cg6502::cg6502()
{
	// Assembles the translation table. It's big, it's ugly, but it yields a convenient way
	// to emulate the 6502. I'm certain there are some "code-golf" strategies to reduce this
	// but I've deliberately kept it verbose for study and alteration

	// It is 16x16 entries. This gives 256 instructions. It is arranged to that the bottom
	// 4 bits of the instruction choose the column, and the top 4 bits choose the row.

	// For convenience to get function pointers to members of this class, I'm using this
	// or else it will be much much larger :D

	// The table is one big initialiser list of initialiser lists...
	using a = cg6502;
	// clang-format off
	lookup = 
	{
		{ "BRK", &a::BRK, &a::IMM, 7 },{ "ORA", &a::ORA, &a::IZX, 6 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "???", &a::NOP, &a::IMP, 3 },{ "ORA", &a::ORA, &a::ZP0, 3 },{ "ASL", &a::ASL, &a::ZP0, 5 },{ "???", &a::XXX, &a::IMP, 5 },{ "PHP", &a::PHP, &a::IMP, 3 },{ "ORA", &a::ORA, &a::IMM, 2 },{ "ASL", &a::ASL, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::NOP, &a::IMP, 4 },{ "ORA", &a::ORA, &a::ABS, 4 },{ "ASL", &a::ASL, &a::ABS, 6 },{ "???", &a::XXX, &a::IMP, 6 },
		{ "BPL", &a::BPL, &a::REL, 2 },{ "ORA", &a::ORA, &a::IZY, 5 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "???", &a::NOP, &a::IMP, 4 },{ "ORA", &a::ORA, &a::ZPX, 4 },{ "ASL", &a::ASL, &a::ZPX, 6 },{ "???", &a::XXX, &a::IMP, 6 },{ "CLC", &a::CLC, &a::IMP, 2 },{ "ORA", &a::ORA, &a::ABY, 4 },{ "???", &a::NOP, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 7 },{ "???", &a::NOP, &a::IMP, 4 },{ "ORA", &a::ORA, &a::ABX, 4 },{ "ASL", &a::ASL, &a::ABX, 7 },{ "???", &a::XXX, &a::IMP, 7 },
		{ "JSR", &a::JSR, &a::ABS, 6 },{ "AND", &a::AND, &a::IZX, 6 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "BIT", &a::BIT, &a::ZP0, 3 },{ "AND", &a::AND, &a::ZP0, 3 },{ "ROL", &a::ROL, &a::ZP0, 5 },{ "???", &a::XXX, &a::IMP, 5 },{ "PLP", &a::PLP, &a::IMP, 4 },{ "AND", &a::AND, &a::IMM, 2 },{ "ROL", &a::ROL, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 2 },{ "BIT", &a::BIT, &a::ABS, 4 },{ "AND", &a::AND, &a::ABS, 4 },{ "ROL", &a::ROL, &a::ABS, 6 },{ "???", &a::XXX, &a::IMP, 6 },
		{ "BMI", &a::BMI, &a::REL, 2 },{ "AND", &a::AND, &a::IZY, 5 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "???", &a::NOP, &a::IMP, 4 },{ "AND", &a::AND, &a::ZPX, 4 },{ "ROL", &a::ROL, &a::ZPX, 6 },{ "???", &a::XXX, &a::IMP, 6 },{ "SEC", &a::SEC, &a::IMP, 2 },{ "AND", &a::AND, &a::ABY, 4 },{ "???", &a::NOP, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 7 },{ "???", &a::NOP, &a::IMP, 4 },{ "AND", &a::AND, &a::ABX, 4 },{ "ROL", &a::ROL, &a::ABX, 7 },{ "???", &a::XXX, &a::IMP, 7 },
		{ "RTI", &a::RTI, &a::IMP, 6 },{ "EOR", &a::EOR, &a::IZX, 6 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "???", &a::NOP, &a::IMP, 3 },{ "EOR", &a::EOR, &a::ZP0, 3 },{ "LSR", &a::LSR, &a::ZP0, 5 },{ "???", &a::XXX, &a::IMP, 5 },{ "PHA", &a::PHA, &a::IMP, 3 },{ "EOR", &a::EOR, &a::IMM, 2 },{ "LSR", &a::LSR, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 2 },{ "JMP", &a::JMP, &a::ABS, 3 },{ "EOR", &a::EOR, &a::ABS, 4 },{ "LSR", &a::LSR, &a::ABS, 6 },{ "???", &a::XXX, &a::IMP, 6 },
		{ "BVC", &a::BVC, &a::REL, 2 },{ "EOR", &a::EOR, &a::IZY, 5 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "???", &a::NOP, &a::IMP, 4 },{ "EOR", &a::EOR, &a::ZPX, 4 },{ "LSR", &a::LSR, &a::ZPX, 6 },{ "???", &a::XXX, &a::IMP, 6 },{ "CLI", &a::CLI, &a::IMP, 2 },{ "EOR", &a::EOR, &a::ABY, 4 },{ "???", &a::NOP, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 7 },{ "???", &a::NOP, &a::IMP, 4 },{ "EOR", &a::EOR, &a::ABX, 4 },{ "LSR", &a::LSR, &a::ABX, 7 },{ "???", &a::XXX, &a::IMP, 7 },
		{ "RTS", &a::RTS, &a::IMP, 6 },{ "ADC", &a::ADC, &a::IZX, 6 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "???", &a::NOP, &a::IMP, 3 },{ "ADC", &a::ADC, &a::ZP0, 3 },{ "ROR", &a::ROR, &a::ZP0, 5 },{ "???", &a::XXX, &a::IMP, 5 },{ "PLA", &a::PLA, &a::IMP, 4 },{ "ADC", &a::ADC, &a::IMM, 2 },{ "ROR", &a::ROR, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 2 },{ "JMP", &a::JMP, &a::IND, 5 },{ "ADC", &a::ADC, &a::ABS, 4 },{ "ROR", &a::ROR, &a::ABS, 6 },{ "???", &a::XXX, &a::IMP, 6 },
		{ "BVS", &a::BVS, &a::REL, 2 },{ "ADC", &a::ADC, &a::IZY, 5 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "???", &a::NOP, &a::IMP, 4 },{ "ADC", &a::ADC, &a::ZPX, 4 },{ "ROR", &a::ROR, &a::ZPX, 6 },{ "???", &a::XXX, &a::IMP, 6 },{ "SEI", &a::SEI, &a::IMP, 2 },{ "ADC", &a::ADC, &a::ABY, 4 },{ "???", &a::NOP, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 7 },{ "???", &a::NOP, &a::IMP, 4 },{ "ADC", &a::ADC, &a::ABX, 4 },{ "ROR", &a::ROR, &a::ABX, 7 },{ "???", &a::XXX, &a::IMP, 7 },
		{ "???", &a::NOP, &a::IMP, 2 },{ "STA", &a::STA, &a::IZX, 6 },{ "???", &a::NOP, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 6 },{ "STY", &a::STY, &a::ZP0, 3 },{ "STA", &a::STA, &a::ZP0, 3 },{ "STX", &a::STX, &a::ZP0, 3 },{ "???", &a::XXX, &a::IMP, 3 },{ "DEY", &a::DEY, &a::IMP, 2 },{ "???", &a::NOP, &a::IMP, 2 },{ "TXA", &a::TXA, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 2 },{ "STY", &a::STY, &a::ABS, 4 },{ "STA", &a::STA, &a::ABS, 4 },{ "STX", &a::STX, &a::ABS, 4 },{ "???", &a::XXX, &a::IMP, 4 },
		{ "BCC", &a::BCC, &a::REL, 2 },{ "STA", &a::STA, &a::IZY, 6 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 6 },{ "STY", &a::STY, &a::ZPX, 4 },{ "STA", &a::STA, &a::ZPX, 4 },{ "STX", &a::STX, &a::ZPY, 4 },{ "???", &a::XXX, &a::IMP, 4 },{ "TYA", &a::TYA, &a::IMP, 2 },{ "STA", &a::STA, &a::ABY, 5 },{ "TXS", &a::TXS, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 5 },{ "???", &a::NOP, &a::IMP, 5 },{ "STA", &a::STA, &a::ABX, 5 },{ "???", &a::XXX, &a::IMP, 5 },{ "???", &a::XXX, &a::IMP, 5 },
		{ "LDY", &a::LDY, &a::IMM, 2 },{ "LDA", &a::LDA, &a::IZX, 6 },{ "LDX", &a::LDX, &a::IMM, 2 },{ "???", &a::XXX, &a::IMP, 6 },{ "LDY", &a::LDY, &a::ZP0, 3 },{ "LDA", &a::LDA, &a::ZP0, 3 },{ "LDX", &a::LDX, &a::ZP0, 3 },{ "???", &a::XXX, &a::IMP, 3 },{ "TAY", &a::TAY, &a::IMP, 2 },{ "LDA", &a::LDA, &a::IMM, 2 },{ "TAX", &a::TAX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 2 },{ "LDY", &a::LDY, &a::ABS, 4 },{ "LDA", &a::LDA, &a::ABS, 4 },{ "LDX", &a::LDX, &a::ABS, 4 },{ "???", &a::XXX, &a::IMP, 4 },
		{ "BCS", &a::BCS, &a::REL, 2 },{ "LDA", &a::LDA, &a::IZY, 5 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 5 },{ "LDY", &a::LDY, &a::ZPX, 4 },{ "LDA", &a::LDA, &a::ZPX, 4 },{ "LDX", &a::LDX, &a::ZPY, 4 },{ "???", &a::XXX, &a::IMP, 4 },{ "CLV", &a::CLV, &a::IMP, 2 },{ "LDA", &a::LDA, &a::ABY, 4 },{ "TSX", &a::TSX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 4 },{ "LDY", &a::LDY, &a::ABX, 4 },{ "LDA", &a::LDA, &a::ABX, 4 },{ "LDX", &a::LDX, &a::ABY, 4 },{ "???", &a::XXX, &a::IMP, 4 },
		{ "CPY", &a::CPY, &a::IMM, 2 },{ "CMP", &a::CMP, &a::IZX, 6 },{ "???", &a::NOP, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "CPY", &a::CPY, &a::ZP0, 3 },{ "CMP", &a::CMP, &a::ZP0, 3 },{ "DEC", &a::DEC, &a::ZP0, 5 },{ "???", &a::XXX, &a::IMP, 5 },{ "INY", &a::INY, &a::IMP, 2 },{ "CMP", &a::CMP, &a::IMM, 2 },{ "DEX", &a::DEX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 2 },{ "CPY", &a::CPY, &a::ABS, 4 },{ "CMP", &a::CMP, &a::ABS, 4 },{ "DEC", &a::DEC, &a::ABS, 6 },{ "???", &a::XXX, &a::IMP, 6 },
		{ "BNE", &a::BNE, &a::REL, 2 },{ "CMP", &a::CMP, &a::IZY, 5 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "???", &a::NOP, &a::IMP, 4 },{ "CMP", &a::CMP, &a::ZPX, 4 },{ "DEC", &a::DEC, &a::ZPX, 6 },{ "???", &a::XXX, &a::IMP, 6 },{ "CLD", &a::CLD, &a::IMP, 2 },{ "CMP", &a::CMP, &a::ABY, 4 },{ "NOP", &a::NOP, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 7 },{ "???", &a::NOP, &a::IMP, 4 },{ "CMP", &a::CMP, &a::ABX, 4 },{ "DEC", &a::DEC, &a::ABX, 7 },{ "???", &a::XXX, &a::IMP, 7 },
		{ "CPX", &a::CPX, &a::IMM, 2 },{ "SBC", &a::SBC, &a::IZX, 6 },{ "???", &a::NOP, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "CPX", &a::CPX, &a::ZP0, 3 },{ "SBC", &a::SBC, &a::ZP0, 3 },{ "INC", &a::INC, &a::ZP0, 5 },{ "???", &a::XXX, &a::IMP, 5 },{ "INX", &a::INX, &a::IMP, 2 },{ "SBC", &a::SBC, &a::IMM, 2 },{ "NOP", &a::NOP, &a::IMP, 2 },{ "???", &a::SBC, &a::IMP, 2 },{ "CPX", &a::CPX, &a::ABS, 4 },{ "SBC", &a::SBC, &a::ABS, 4 },{ "INC", &a::INC, &a::ABS, 6 },{ "???", &a::XXX, &a::IMP, 6 },
		{ "BEQ", &a::BEQ, &a::REL, 2 },{ "SBC", &a::SBC, &a::IZY, 5 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "???", &a::NOP, &a::IMP, 4 },{ "SBC", &a::SBC, &a::ZPX, 4 },{ "INC", &a::INC, &a::ZPX, 6 },{ "???", &a::XXX, &a::IMP, 6 },{ "SED", &a::SED, &a::IMP, 2 },{ "SBC", &a::SBC, &a::ABY, 4 },{ "NOP", &a::NOP, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 7 },{ "???", &a::NOP, &a::IMP, 4 },{ "SBC", &a::SBC, &a::ABX, 4 },{ "INC", &a::INC, &a::ABX, 7 },{ "???", &a::XXX, &a::IMP, 7 },
	};
	// clang-format on

	X = 0x00;
	Y = 0x00;
	A = 0x00;
	P = 0x00;
	set_flag(C, 1);
	set_flag(Z, 1);
	set_flag(D, 1);
}

cg6502::~cg6502()
{
}

uint8_t cg6502::read(uint16_t a)
{
	return bus->read(a, false);
}

void cg6502::write(uint16_t a, uint8_t d)
{
	bus->write(a, d);
}

void cg6502::clock()
{
	if (cycles == 0)
	{
		opcode = read(PC++);
		cycles = lookup[opcode].cycles;

		uint8_t additional_cycle_1 = (this->*lookup[opcode].addrmode)();
		uint8_t additional_cycle_2 = (this->*lookup[opcode].operate)();

		cycles += (additional_cycle_1 & additional_cycle_2);
	}
	cycles--;
}

void cg6502::reset()
{
	// cycles = 6;
	set_flag(I, 1);
	uint8_t lo = read(0xFFFC);
	uint8_t hi = read(0xFFFD);
	PC		   = ((uint16_t)hi << 8) | lo;

	// Reset internal registers
	A = 0;
	X = 0;
	Y = 0;
	S = 0xFD;
	P = 0x00 | U;

	// Clear internal helper variables
	addr_rel = 0x0000;
	addr_abs = 0x0000;
	fetched = 0x00;

	// Reset takes time
	cycles = 8;
}
void cg6502::irq()
{
	if (get_flag(I) == 0)
	{
		write(0x0100 + S--, (PC >> 8) & 0x00FF);
		write(0x0100 + S--, PC & 0x00FF);

		// Then Push the status register to the stack
		set_flag(B, 0);
		set_flag(U, 1);
		set_flag(I, 1);
		write(0x0100 + S--, P);

		// Read new program counter location from fixed address
		addr_abs	= 0xFFFE;
		uint16_t lo = read(addr_abs + 0);
		uint16_t hi = read(addr_abs + 1);
		PC			= (hi << 8) | lo;

		// IRQs take time
		cycles = 7;
	}
}
void cg6502::nmi()
{
	write(0x0100 + S--, (PC >> 8) & 0x00FF);
	write(0x0100 + S--, PC & 0x00FF);

	set_flag(B, 0);
	set_flag(U, 1);
	set_flag(I, 1);
	write(0x0100 + S--, P);

	addr_abs	= 0xFFFA;
	uint16_t lo = read(addr_abs + 0);
	uint16_t hi = read(addr_abs + 1);
	PC			= (hi << 8) | lo;

	cycles = 8;
}

// Something something
uint8_t cg6502::IMP()
{
	fetched = A;
	return 0;
}
uint8_t cg6502::IMM()
{
	addr_abs = PC++;
	return 0;
}
uint8_t cg6502::ZP0()
{
	addr_abs = read(PC++);
	addr_abs &= 0x00FF;
	return 0;
}
uint8_t cg6502::ZPX()
{
	addr_abs = (read(PC++) + X);
	addr_abs &= 0x00FF;
	return 0;
}
uint8_t cg6502::ZPY()
{
	addr_abs = (read(PC++) + Y);
	addr_abs &= 0x00FF;
	return 0;
}
uint8_t cg6502::ABS()
{
	uint8_t lo = read(PC++);
	uint8_t hi = read(PC++);
	addr_abs   = ((uint16_t)hi << 8) + lo;
	return 0;
}
uint8_t cg6502::ABX()
{
	uint8_t lo = read(PC++);
	uint8_t hi = read(PC++);

	addr_abs = ((uint16_t)hi << 8) | lo;
	addr_abs += X;

	if ((addr_abs & 0xFF00) != (hi << 8))
		return 1;
	else
		return 0;
}
uint8_t cg6502::ABY()
{
	uint8_t lo = read(PC++);
	uint8_t hi = read(PC++);

	addr_abs = ((uint16_t)hi << 8) | lo;
	addr_abs += Y;

	if ((addr_abs & 0xFF00) != (hi << 8))
		return 1;
	else
		return 0;
}
uint8_t cg6502::IND()
{
	uint8_t ptr_low	 = read(PC++);
	uint8_t ptr_high = read(PC++);

	uint16_t ptr = (((uint16_t)ptr_high << 8) | ptr_low);

	addr_abs = (read(ptr + 1) << 8) | read(ptr + 0);
	// if (ptr_low == 0x00FF)	// Simulate page boundary hardware bug
	// {
	// 	addr_abs = (read(ptr & 0xFF00) << 8) | read(ptr + 0);
	// }
	// else  // Behave normally
	// {
	// 	addr_abs = (read(ptr + 1) << 8) | read(ptr + 0);
	// }
	return 0;
}
uint8_t cg6502::IZX()
{
	uint16_t t = (read(PC++) + X);

	uint16_t lo = read(t & 0x00FF);
	uint16_t hi = read((t + 1) & 0x00FF);

	addr_abs = (hi << 8) | lo;
	return 0;
}
uint8_t cg6502::IZY()
{
	uint16_t t = read(PC++);

	uint16_t lo = read(t & 0x00FF);
	uint16_t hi = read((t + 1) & 0x00FF);

	addr_abs = (hi << 8) | lo;
	addr_abs += Y;

	if ((addr_abs & 0xFF00) != (hi << 8))
		return 1;
	else
		return 0;
}
uint8_t cg6502::REL()
{
	addr_rel = read(PC++);
	// printf("read(%04x) = addr_rel: %d, hex: %04x \n", PC - 1, (int16_t)addr_rel, addr_rel);
	if (addr_rel & 0x80)
	{
		// printf("Negative number!\n");
		addr_rel |= 0xFF00;
	}
	return 0;
}

// This function sources the data used by the instruction into
// a convenient numeric variable. Some instructions dont have to
// fetch data as the source is implied by the instruction. For example
// "INX" increments the X register. There is no additional data
// required. For all other addressing modes, the data resides at
// the location held within addr_abs, so it is read from there.
// Immediate adress mode exploits this slightly, as that has
// set addr_abs = pc + 1, so it fetches the data from the
// next byte for example "LDA $FF" just loads the accumulator with
// 256, i.e. no far reaching memory fetch is required. "fetched"
// is a variable global to the CPU, and is set by calling this
// function. It also returns it for convenience.
uint8_t cg6502::fetch()
{
	if (!(lookup[opcode].addrmode == &cg6502::IMP))
		fetched = read(addr_abs);
	return fetched;
}

// Load Accumulator
uint8_t cg6502::LDA()
{
	fetch();
	A = fetched;
	set_flag(Z, A == 0x00);
	set_flag(N, A & 0x80);
	return 0;
}
// Load X Register
uint8_t cg6502::LDX()
{
	fetch();
	X = fetched;
	set_flag(Z, X == 0x00);
	set_flag(N, X & 0x80);
	return 1;
}
// Load Y Register
uint8_t cg6502::LDY()
{
	fetch();
	Y = fetched;
	set_flag(Z, Y == 0x00);
	set_flag(N, Y & 0x80);
	return 1;
}
// Store Accumulator
uint8_t cg6502::STA()
{
	write(addr_abs, A);
	return 1;
}
// Store X Register
uint8_t cg6502::STX()
{
	write(addr_abs, X);
	return 0;
}
// Store Y Register
uint8_t cg6502::STY()
{
	write(addr_abs, Y);
	return 0;
}
// Transfer accumulator to X
uint8_t cg6502::TAX()
{
	X = A;
	set_flag(Z, X == 0x00);
	set_flag(N, X & 0x80);
	return 0;
}
// Transfer accumulator to Y
uint8_t cg6502::TAY()
{
	Y = A;
	set_flag(Z, Y == 0x00);
	set_flag(N, Y & 0x80);
	return 0;
}
// Transfer X to accumulator
uint8_t cg6502::TXA()
{
	A = X;
	set_flag(Z, A == 0x00);
	set_flag(N, A & 0x80);
	return 0;
}
// Transfer Y to accumulator
uint8_t cg6502::TYA()
{
	A = Y;
	set_flag(Z, A == 0x00);
	set_flag(N, A & 0x80);
	return 0;
}
// Transfer stack pointer to X
uint8_t cg6502::TSX()
{
	X = S;
	set_flag(Z, X == 0x00);
	set_flag(N, X & 0x80);
	return 0;
}
// Transfer X to stack pointer
uint8_t cg6502::TXS()
{
	S = X;
	set_flag(Z, S == 0x00);
	set_flag(N, S & 0x80);
	return 0;
}
// Push accumulator on stack
uint8_t cg6502::PHA()
{
	write(0x0100 + S--, A);
	return 0;
}
// Push processor status on stack
uint8_t cg6502::PHP()
{
	write(0x0100 + S--, P | B | U);
	set_flag(B, 0);
	set_flag(U, 0);
	return 0;
}
// Pull accumulator from stack
uint8_t cg6502::PLA()
{
	A = read(0x0100 + ++S);
	set_flag(Z, A == 0x00);
	set_flag(N, A & 0x80);
	return 0;
}
// Pull processor status from stack
uint8_t cg6502::PLP()
{
	P = read(0x0100 + ++S);
	set_flag(U, 1);
	return 0;
}
// Logical AND
uint8_t cg6502::AND()
{
	fetch();
	A &= fetched;
	set_flag(Z, A == 0x00);
	set_flag(N, A & 0x80);
	return 0;
}
// Exclusive OR
uint8_t cg6502::EOR()
{
	fetch();
	A ^= fetched;
	set_flag(Z, A == 0x00);
	set_flag(N, A & 0x80);
	return 0;
}
// Logical Inclusive OR
uint8_t cg6502::ORA()
{
	fetch();
	A |= fetched;
	set_flag(Z, A == 0x00);
	set_flag(N, A & 0x80);
	return 0;
}
// Bit Test
uint8_t cg6502::BIT()
{
	fetch();
	set_flag(Z, (A & fetched) == 0x00);
	set_flag(V, fetched & 0x40);
	set_flag(N, fetched & 0x80);
	return 0;
}
// Add with Carry
uint8_t cg6502::ADC()
{
	fetch();
	uint16_t temp = (uint16_t)A + (uint16_t)fetched + (uint16_t)get_flag(C);

	set_flag(C, (temp > 0xFF));
	set_flag(Z, (temp & 0x00FF) == 0);
	set_flag(N, temp & 0x80);
	// The overflow flag is set when the sign of the addends is the same and
	// differs from the sign of the sum
	set_flag(V, (~((uint16_t)A ^ (uint16_t)fetched) & ((uint16_t)A ^ (uint16_t)temp) & 0x0080));
	A = temp & 0x00FF;
	return 1;
}
// Subtract with Carry
uint8_t cg6502::SBC()
{
	fetch();
	uint16_t temp = (uint16_t)A + (~((uint16_t)fetched) + 1) + (uint16_t)get_flag(C);

	set_flag(C, (temp > 0xFF));
	set_flag(Z, (temp & 0x00FF) == 0);
	set_flag(N, temp & 0x80);
	// The overflow flag is set when the sign of the addends is the same and
	// differs from the sign of the sum
	set_flag(V, (~((uint16_t)A ^ (uint16_t)fetched) & ((uint16_t)A ^ (uint16_t)temp) & 0x0080));
	A = temp & 0x00FF;
	return 1;
}
// Compare accumulator
uint8_t cg6502::CMP()
{
	fetch();
	uint16_t temp = (uint16_t)A + (~((uint16_t)fetched) + 1);
	// printf("temp: %04x\t", temp);
	// printf("C1: %d C2: %d\n", ((temp & 0x00FF) > 0xFF), (temp & 0x00FF) == 0);
	set_flag(C, ((temp & 0x00FF) > 0xFF) || (temp & 0x00FF) == 0);
	set_flag(Z, (temp & 0x00FF) == 0);
	set_flag(N, temp & 0x80);
	return 0;
}
// Compare X register
uint8_t cg6502::CPX()
{
	fetch();
	uint16_t temp = (uint16_t)X - (uint16_t)fetched;
	set_flag(C, X >= fetched);
	set_flag(Z, (temp & 0x00FF) == 0x0000);
	set_flag(N, temp & 0x0080);
	return 0;
}
// Compare Y register
uint8_t cg6502::CPY()
{
	fetch();
	uint16_t temp = (uint16_t)Y - (uint16_t)fetched;
	set_flag(C, Y >= fetched);
	set_flag(Z, (temp & 0x00FF) == 0x0000);
	set_flag(N, temp & 0x0080);
	return 0;
}
// Increment a memory location
uint8_t cg6502::INC()
{
	fetch();
	fetched++;
	write(addr_abs, fetched);
	set_flag(Z, fetched == 0x00);
	set_flag(N, fetched & 0x80);
	return 1;
}
// Increment the X register
uint8_t cg6502::INX()
{
	X++;
	set_flag(Z, X == 0x00);
	set_flag(N, X & 0x80);
	return 0;
}
// Increment the Y register
uint8_t cg6502::INY()
{
	Y++;
	set_flag(Z, Y == 0x00);
	set_flag(N, Y & 0x80);
	return 0;
}
// Decrement a memory location
uint8_t cg6502::DEC()
{
	fetch();
	fetched--;
	write(addr_abs, fetched);
	set_flag(Z, fetched == 0x00);
	set_flag(N, fetched & 0x80);
	return 0;
}
// Decrement the X register
uint8_t cg6502::DEX()
{
	X--;
	set_flag(Z, X == 0x00);
	set_flag(N, X & 0x80);
	return 0;
}
// Decrement the Y register
uint8_t cg6502::DEY()
{
	Y--;
	set_flag(Z, Y == 0x00);
	set_flag(N, Y & 0x80);
	return 0;
}
// Arithmetic Shift Left
uint8_t cg6502::ASL()
{
	fetch();
	set_flag(C, fetched & 0x0080);
	uint16_t temp = fetched << 1;
	set_flag(Z, (temp & 0x00FF) == 0x0000);
	set_flag(N, temp & 0x0080);
	if (lookup[opcode].addrmode == &cg6502::IMP)
		A = temp & 0x00FF;
	else
		write(addr_abs, temp & 0x00FF);
	return 0;
}
// Logical Shift Right
uint8_t cg6502::LSR()
{
	fetch();
	set_flag(C, fetched & 0x0001);
	uint16_t temp = fetched >> 1;
	set_flag(Z, (temp & 0x00FF) == 0x0000);
	set_flag(N, temp & 0x0080);
	if (lookup[opcode].addrmode == &cg6502::IMP)
		A = temp & 0x00FF;
	else
		write(addr_abs, temp & 0x00FF);
	return 0;
}
// Rotate Left
uint8_t cg6502::ROL()
{
	fetch();
	uint16_t temp = (uint16_t)(fetched << 1) | get_flag(C);
	set_flag(C, temp & 0xFF00);
	set_flag(Z, (temp & 0x00FF) == 0x0000);
	set_flag(N, temp & 0x0080);
	if (lookup[opcode].addrmode == &cg6502::IMP)
		A = temp & 0x00FF;
	else
		write(addr_abs, temp & 0x00FF);
	return 0;
}
// Rotate Right
uint8_t cg6502::ROR()
{
	fetch();
	uint16_t temp = (uint16_t)(get_flag(C) << 7) | (fetched >> 1);
	set_flag(C, fetched & 0x01);
	set_flag(Z, (temp & 0x00FF) == 0x00);
	set_flag(N, temp & 0x0080);
	if (lookup[opcode].addrmode == &cg6502::IMP)
		A = temp & 0x00FF;
	else
		write(addr_abs, temp & 0x00FF);
	return 0;
}
// Jump to another location
uint8_t cg6502::JMP()
{
	PC = addr_abs;
	return 0;
}
// Jump to a subroutine
uint8_t cg6502::JSR()
{
	PC--;
	write(0x0100 + S--, (PC >> 8) & 0x00FF);
	write(0x0100 + S--, PC & 0x00FF);
	PC = addr_abs;
	return 0;
}
// Return from subroutine
uint8_t cg6502::RTS()
{
	PC = (uint16_t)read(0x0100 + ++S);
	PC |= (uint16_t)read(0x0100 + ++S) << 8;
	PC++;
	return 0;
}
// Return from Interrupt
uint8_t cg6502::RTI()
{
	P = read(0x0100 + ++S);
	P &= ~B;
	P &= ~U;

	PC |= (uint16_t)read(0x0100 + ++S) << 8;
	return 0;
}
// Branch if carry flag clear
uint8_t cg6502::BCC()
{
	if (get_flag(C) == 0)
	{
		cycles++;
		addr_abs = PC + addr_rel;

		if ((addr_abs & 0xFF00) != (PC & 0xFF00))
			cycles++;

		PC = addr_abs;
	}
	return 0;
}
// Branch if carry flag set
uint8_t cg6502::BCS()
{
	if (get_flag(C) == 1)
	{
		cycles++;
		addr_abs = PC + addr_rel;

		if ((addr_abs & 0xFF00) != (PC & 0xFF00))
			cycles++;

		PC = addr_abs;
	}
	return 0;
}
// Branch if zero flag set
uint8_t cg6502::BEQ()
{
	if (get_flag(Z) == 1)
	{
		cycles++;
		addr_abs = PC + addr_rel;
		// printf("abs: %d PC: %d  rel: %d\n", addr_abs, PC, (int16_t)addr_rel);

		if ((addr_abs & 0xFF00) != (PC & 0xFF00))
			cycles++;

		PC = addr_abs;
	}
	return 0;
}
// Branch if negative flag set
uint8_t cg6502::BMI()
{
	if (get_flag(N) == 1)
	{
		cycles++;
		addr_abs = PC + addr_rel;

		if ((addr_abs & 0xFF00) != (PC & 0xFF00))
			cycles++;

		PC = addr_abs;
	}
	return 0;
}
// Branch if zero flag clear
uint8_t cg6502::BNE()
{
	if (get_flag(Z) == 0)
	{
		cycles++;
		addr_abs = PC + addr_rel;

		if ((addr_abs & 0xFF00) != (PC & 0xFF00))
			cycles++;

		PC = addr_abs;
	}
	return 0;
}
// Branch if negative flag clear
uint8_t cg6502::BPL()
{
	if (get_flag(N) == 0)
	{
		cycles++;
		addr_abs = PC + addr_rel;

		if ((addr_abs & 0xFF00) != (PC & 0xFF00))
			cycles++;

		PC = addr_abs;
	}
	return 0;
}
// Branch if overflow flag clear
uint8_t cg6502::BVC()
{
	if (get_flag(V) == 0)
	{
		cycles++;
		addr_abs = PC + addr_rel;

		if ((addr_abs & 0xFF00) != (PC & 0xFF00))
			cycles++;

		PC = addr_abs;
	}
	return 0;
}
// Branch if overflow flag set
uint8_t cg6502::BVS()
{
	if (get_flag(V) == 1)
	{
		cycles++;
		addr_abs = PC + addr_rel;

		if ((addr_abs & 0xFF00) != (PC & 0xFF00))
			cycles++;

		PC = addr_abs;
	}
	return 0;
}
// Clear carry flag
uint8_t cg6502::CLC()
{
	set_flag(C, 0);
	return 0;
}
// Clear decimal mode flag
uint8_t cg6502::CLD()
{
	set_flag(D, 0);
	return 0;
}
// Clear interrupt disable flag
uint8_t cg6502::CLI()
{
	set_flag(I, 0);
	return 0;
}
// Clear overflow flag
uint8_t cg6502::CLV()
{
	set_flag(V, 0);
	return 0;
}
// Set carry flag
uint8_t cg6502::SEC()
{
	set_flag(C, 1);
	return 0;
}
// Set decimal mode flag
uint8_t cg6502::SED()
{
	set_flag(D, 1);
	return 0;
}
// Set interrupt disable flag
uint8_t cg6502::SEI()
{
	set_flag(I, 1);
	return 0;
}
// Force an interrupt
uint8_t cg6502::BRK()
{
	PC++;

	set_flag(I, 1);
	write(0x0100 + S--, (PC >> 8) & 0x00FF);
	write(0x0100 + S--, PC & 0x00FF);

	set_flag(B, 1);
	write(0x0100 + S--, P);
	set_flag(B, 0);

	PC = (uint16_t)read(0xFFFE) | ((uint16_t)read(0xFFFF) << 8);
	return 0;
}
// No Operation
uint8_t cg6502::NOP()
{
	return 0;
}
// Error reading instruction
uint8_t cg6502::XXX()
{
	return 0;
}
bool cg6502::complete()
{
	return cycles == 0;
}

// This is the disassembly function. Its workings are not required for emulation.
// It is merely a convenience function to turn the binary instruction code into
// human readable form. Its included as part of the emulator because it can take
// advantage of many of the CPUs internal operations to do this.
std::map<uint16_t, std::string> cg6502::disassemble(uint16_t nStart, uint16_t nStop)
{
	uint32_t addr = nStart;
	uint8_t value = 0x00, lo = 0x00, hi = 0x00;
	std::map<uint16_t, std::string> mapLines;
	uint16_t line_addr = 0;

	// A convenient utility to convert variables into
	// hex strings because "modern C++"'s method with
	// streams is atrocious
	auto hex = [](uint32_t n, uint8_t d)
	{
		std::string s(d, '0');
		for (int i = d - 1; i >= 0; i--, n >>= 4)
			s[i] = "0123456789ABCDEF"[n & 0xF];
		return s;
	};

	// Starting at the specified address we read an instruction
	// byte, which in turn yields information from the lookup table
	// as to how many additional bytes we need to read and what the
	// addressing mode is. I need this info to assemble human readable
	// syntax, which is different depending upon the addressing mode

	// As the instruction is decoded, a std::string is assembled
	// with the readable output
	while (addr <= (uint32_t)nStop)
	{
		line_addr = addr;

		// Prefix line with instruction address
		std::string sInst = "$" + hex(addr, 4) + ": ";

		// Read instruction, and get its readable name
		uint8_t opcode = bus->read(addr, true);
		addr++;
		sInst += lookup[opcode].name + " ";

		// Get oprands from desired locations, and form the
		// instruction based upon its addressing mode. These
		// routines mimmick the actual fetch routine of the
		// 6502 in order to get accurate data as part of the
		// instruction
		if (lookup[opcode].addrmode == &cg6502::IMP)
		{
			sInst += " {IMP}";
		}
		else if (lookup[opcode].addrmode == &cg6502::IMM)
		{
			value = bus->read(addr, true);
			addr++;
			sInst += "#$" + hex(value, 2) + " {IMM}";
		}
		else if (lookup[opcode].addrmode == &cg6502::ZP0)
		{
			lo = bus->read(addr, true);
			addr++;
			hi = 0x00;
			sInst += "$" + hex(lo, 2) + " {ZP0}";
		}
		else if (lookup[opcode].addrmode == &cg6502::ZPX)
		{
			lo = bus->read(addr, true);
			addr++;
			hi = 0x00;
			sInst += "$" + hex(lo, 2) + ", X {ZPX}";
		}
		else if (lookup[opcode].addrmode == &cg6502::ZPY)
		{
			lo = bus->read(addr, true);
			addr++;
			hi = 0x00;
			sInst += "$" + hex(lo, 2) + ", Y {ZPY}";
		}
		else if (lookup[opcode].addrmode == &cg6502::IZX)
		{
			lo = bus->read(addr, true);
			addr++;
			hi = 0x00;
			sInst += "($" + hex(lo, 2) + ", X) {IZX}";
		}
		else if (lookup[opcode].addrmode == &cg6502::IZY)
		{
			lo = bus->read(addr, true);
			addr++;
			hi = 0x00;
			sInst += "($" + hex(lo, 2) + "), Y {IZY}";
		}
		else if (lookup[opcode].addrmode == &cg6502::ABS)
		{
			lo = bus->read(addr, true);
			addr++;
			hi = bus->read(addr, true);
			addr++;
			sInst += "$" + hex((uint16_t)(hi << 8) | lo, 4) + " {ABS}";
		}
		else if (lookup[opcode].addrmode == &cg6502::ABX)
		{
			lo = bus->read(addr, true);
			addr++;
			hi = bus->read(addr, true);
			addr++;
			sInst += "$" + hex((uint16_t)(hi << 8) | lo, 4) + ", X {ABX}";
		}
		else if (lookup[opcode].addrmode == &cg6502::ABY)
		{
			lo = bus->read(addr, true);
			addr++;
			hi = bus->read(addr, true);
			addr++;
			sInst += "$" + hex((uint16_t)(hi << 8) | lo, 4) + ", Y {ABY}";
		}
		else if (lookup[opcode].addrmode == &cg6502::IND)
		{
			lo = bus->read(addr, true);
			addr++;
			hi = bus->read(addr, true);
			addr++;
			sInst += "($" + hex((uint16_t)(hi << 8) | lo, 4) + ") {IND}";
		}
		else if (lookup[opcode].addrmode == &cg6502::REL)
		{
			value = bus->read(addr, true);
			addr++;
			sInst += "$" + hex(value, 2) + " [$" + hex(addr + (int8_t)value, 4) + "] {REL}";
		}

		// Add the formed string to a std::map, using the instruction's
		// address as the key. This makes it convenient to look for later
		// as the instructions are variable in length, so a straight up
		// incremental index is not sufficient.
		mapLines[line_addr] = sInst;
	}

	return mapLines;
}