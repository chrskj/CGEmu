#pragma once

#include <cstdint>
#include <vector>
#include <string>
#include <map>

class Bus;

class cg6502
{
   public:
	cg6502();
	~cg6502();

	enum Status_flag
	{
		C = (1 << 0),  // Carry
		Z = (1 << 1),  // Zero
		I = (1 << 2),  // IRQ Disable
		D = (1 << 3),  // Decimal Mode
		B = (1 << 4),  // BRK Command
		U = (1 << 5),  // Not Used
		V = (1 << 6),  // Overflow
		N = (1 << 7),  // Negative
	};

	// CPU Core registers,
	uint16_t PC;  // Program Counter
	uint8_t S;	  // Stack Pointer
	uint8_t X;	  // X register
	uint8_t Y;	  // Y register
	uint8_t A;	  // Accumulator
	uint8_t P;	  // Processor Status Register

	void clock();
	void reset();
	void irq();
	void nmi();

	// Indicates the current instruction has completed by returning true. This is
	// a utility function to enable "step-by-step" execution, without manually
	// clocking every cycle
	bool complete();

	void connect_to_bus(Bus *n) { bus = n; }

	// Produces a map of strings, with keys equivalent to instruction start locations
	// in memory, for the specified address range
	std::map<uint16_t, std::string> disassemble(uint16_t nStart, uint16_t nStop);

	uint8_t opcode = 0x00;
	struct INSTRUCTION
	{
		std::string name;
		uint8_t (cg6502::*operate)(void);
		uint8_t (cg6502::*addrmode)(void);
		uint8_t cycles;
	};

	std::vector<INSTRUCTION> lookup;

   private:
	// Addressing modes
	// clang-format off
	uint8_t ACC(); uint8_t IMP(); uint8_t IMM();
	uint8_t ZP0(); uint8_t ZPX(); uint8_t ZPY(); 
	uint8_t ABS(); uint8_t ABX(); uint8_t ABY(); 
	uint8_t IND(); uint8_t IZX(); uint8_t IZY();
	uint8_t REL(); 

	// Instruction set
	uint8_t LDA(); uint8_t LDX(); uint8_t LDY(); uint8_t STA();
	uint8_t STX(); uint8_t STY(); uint8_t TAX(); uint8_t TAY();
	uint8_t TXA(); uint8_t TYA(); uint8_t TSX(); uint8_t TXS();
	uint8_t PHA(); uint8_t PHP(); uint8_t PLA(); uint8_t PLP();
	uint8_t AND(); uint8_t EOR(); uint8_t ORA(); uint8_t BIT();
	uint8_t ADC(); uint8_t SBC(); uint8_t CMP(); uint8_t CPX();
	uint8_t CPY(); uint8_t INC(); uint8_t INX(); uint8_t INY();
	uint8_t DEC(); uint8_t DEX(); uint8_t DEY(); uint8_t ASL();
	uint8_t LSR(); uint8_t ROL(); uint8_t ROR(); uint8_t JMP();
	uint8_t JSR(); uint8_t RTS(); uint8_t BCC(); uint8_t BCS();
	uint8_t BEQ(); uint8_t BMI(); uint8_t BNE(); uint8_t BPL();
	uint8_t BVC(); uint8_t BVS(); uint8_t CLC(); uint8_t CLD();
	uint8_t CLI(); uint8_t CLV(); uint8_t SEC(); uint8_t SED();
	uint8_t SEI(); uint8_t BRK(); uint8_t NOP(); uint8_t RTI();
	uint8_t XXX();
	// clang-format on

	// Internal helper functions
	uint8_t fetched	  = 0x00;
	uint16_t addr_abs = 0x0000;
	uint16_t addr_rel = 0x0000;
	uint8_t cycles	  = 0;

	Bus *bus = nullptr;

	uint8_t read(uint16_t a);
	void write(uint16_t a, uint8_t d);

	uint8_t fetch();

	uint8_t get_flag(Status_flag f)
	{
		return !!(P & f);  // '!!' to make sure this returns 0 or 1
	}

	void set_flag(Status_flag f, bool v)
	{
		if (v)
			P |= f;
		else
			P &= ~f;
	}
	void toggle_flag(Status_flag f)
	{
		P ^= f;
	}

	// struct INSTRUCTION
	// {
	// 	std::string name;
	// 	uint8_t (cg6502::*operate)(void)  = nullptr;
	// 	uint8_t (cg6502::*addrmode)(void) = nullptr;
	// 	uint8_t cycles					  = 0;
	// };

	// std::vector<INSTRUCTION> lookup;
};
