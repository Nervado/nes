#include <vector>

#include "../Bus/Bus.h"
#include "Processor.h"



Processor::Processor()
{
  using a = Processor;

  // Assembles the translation table. It's big, it's ugly, but it yields a convenient way
  // to emulate the 6502. I'm certain there are some "code-golf" strategies to reduce this
  // but I've deliberately kept it verbose for study and alteration

  // It is 16x16 entries. This gives 256 instructions. It is arranged to that the bottom
  // 4 bits of the instruction choose the column, and the top 4 bits choose the row.

  // For convenience to get function pointers to members of this class, I'm using this
  // or else it will be much much larger :D

  // The table is one big initialiser list of initialiser lists...
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

  
}

Processor::~Processor()
{
  // do nothing
}

///////////////////////////////////////////////////////////////////////////////
// BUS CONNECTIVITY

// Reads an 8-bit byte from the bus, located at the specified 16-bit address
uint8_t Processor::read(uint16_t a)
{
	// In normal operation "read only" is set to false. This may seem odd. Some
	// devices on the bus may change state when they are read from, and this 
	// is intentional under normal circumstances. However the disassembler will
	// want to read the data at an address without changing the state of the
	// devices on the bus
	return bus->read(a, false);
}

// Writes a byte to the bus at the specified address
void Processor::write(uint16_t a, uint8_t d)
{
	bus->write(a, d);
}



///////////////////////////////////////////////////////////////////////////////
// EXTERNAL INPUTS

// Forces the 6502 into a known state. This is hard-wired inside the CPU. The
// registers are set to 0x00, the status register is cleared except for unused
// bit which remains at 1. An absolute address is read from location 0xFFFC
// which contains a second address that the program counter is set to. This 
// allows the programmer to jump to a known and programmable location in the
// memory to start executing from. Typically the programmer would set the value
// at location 0xFFFC at compile time.
void Processor::reset()
{
	// Get address to set program counter to
	addr_abs = 0xFFFC;
	uint16_t lo = read(addr_abs + 0);
	uint16_t hi = read(addr_abs + 1);

	// Set it
	pc = (hi << 8) | lo;

	// Reset internal registers
	a = 0;
	x = 0;
	y = 0;
	stkp = 0xFD;
	status = 0x00 | U;

	// Clear internal helper variables
	addr_rel = 0x0000;
	addr_abs = 0x0000;
	fetched = 0x00;

	// Reset takes time
	cycles = 8;
}


// Interrupt requests are a complex operation and only happen if the
// "disable interrupt" flag is 0. IRQs can happen at any time, but
// you dont want them to be destructive to the operation of the running 
// program. Therefore the current instruction is allowed to finish
// (which I facilitate by doing the whole thing when cycles == 0) and 
// then the current program counter is stored on the stack. Then the
// current status register is stored on the stack. When the routine
// that services the interrupt has finished, the status register
// and program counter can be restored to how they where before it 
// occurred. This is impemented by the "RTI" instruction. Once the IRQ
// has happened, in a similar way to a reset, a programmable address
// is read form hard coded location 0xFFFE, which is subsequently
// set to the program counter.
void Processor::irq()
{
	// If interrupts are allowed
	if (GetFlag(I) == 0)
	{
		// Push the program counter to the stack. It's 16-bits dont
		// forget so that takes two pushes
		write(0x0100 + stkp, (pc >> 8) & 0x00FF);
		stkp--;
		write(0x0100 + stkp, pc & 0x00FF);
		stkp--;

		// Then Push the status register to the stack
		SetFlag(B, 0);
		SetFlag(U, 1);
		SetFlag(I, 1);
		write(0x0100 + stkp, status);
		stkp--;

		// Read new program counter location from fixed address
		addr_abs = 0xFFFE;
		uint16_t lo = read(addr_abs + 0);
		uint16_t hi = read(addr_abs + 1);
		pc = (hi << 8) | lo;

		// IRQs take time
		cycles = 7;
	}
}


// A Non-Maskable Interrupt cannot be ignored. It behaves in exactly the
// same way as a regular IRQ, but reads the new program counter address
// form location 0xFFFA.
void Processor::nmi()
{
	write(0x0100 + stkp, (pc >> 8) & 0x00FF);
	stkp--;
	write(0x0100 + stkp, pc & 0x00FF);
	stkp--;

	SetFlag(B, 0);
	SetFlag(U, 1);
	SetFlag(I, 1);
	write(0x0100 + stkp, status);
	stkp--;

	addr_abs = 0xFFFA;
	uint16_t lo = read(addr_abs + 0);
	uint16_t hi = read(addr_abs + 1);
	pc = (hi << 8) | lo;

	cycles = 8;
}

// Perform one clock cycles worth of emulation
void Processor::clock()
{
	// Each instruction requires a variable number of clock cycles to execute.
	// In my emulation, I only care about the final result and so I perform
	// the entire computation in one hit. In hardware, each clock cycle would
	// perform "microcode" style transformations of the CPUs state.
	//
	// To remain compliant with connected devices, it's important that the 
	// emulation also takes "time" in order to execute instructions, so I
	// implement that delay by simply counting down the cycles required by 
	// the instruction. When it reaches 0, the instruction is complete, and
	// the next one is ready to be executed.
	if (cycles == 0)
	{
		// Read next instruction byte. This 8-bit value is used to index
		// the translation table to get the relevant information about
		// how to implement the instruction
		opcode = read(pc);

#ifdef LOGMODE
		uint16_t log_pc = pc;
#endif
		
		// Always set the unused status flag bit to 1
		SetFlag(U, true);
		
		// Increment program counter, we read the opcode byte
		pc++;

		// Get Starting number of cycles
		cycles = lookup[opcode].cycles;

		// Perform fetch of intermmediate data using the
		// required addressing mode
		uint8_t additional_cycle1 = (this->*lookup[opcode].addrmode)();

		// Perform operation
		uint8_t additional_cycle2 = (this->*lookup[opcode].operate)();

		// The addressmode and opcode may have altered the number
		// of cycles this instruction requires before its completed
		cycles += (additional_cycle1 & additional_cycle2);

		// Always set the unused status flag bit to 1
		SetFlag(U, true);

#ifdef LOGMODE
		// This logger dumps every cycle the entire processor state for analysis.
		// This can be used for debugging the emulation, but has little utility
		// during emulation. Its also very slow, so only use if you have to.
		if (logfile == nullptr)	logfile = fopen("Processor.txt", "wt");
		if (logfile != nullptr)
		{
			fprintf(logfile, "%10d:%02d PC:%04X %s A:%02X X:%02X Y:%02X %s%s%s%s%s%s%s%s STKP:%02X\n",
				clock_count, 0, log_pc, "XXX", a, x, y,	
				GetFlag(N) ? "N" : ".",	GetFlag(V) ? "V" : ".",	GetFlag(U) ? "U" : ".",	
				GetFlag(B) ? "B" : ".",	GetFlag(D) ? "D" : ".",	GetFlag(I) ? "I" : ".",	
				GetFlag(Z) ? "Z" : ".",	GetFlag(C) ? "C" : ".",	stkp);
		}
#endif
	}
	
	// Increment global clock count - This is actually unused unless logging is enabled
	// but I've kept it in because its a handy watch variable for debugging
	clock_count++;

	// Decrement the number of cycles remaining for this instruction
	cycles--;
}



///////////////////////////////////////////////////////////////////////////////
// FLAG FUNCTIONS

// Returns the value of a specific bit of the status register
uint8_t Processor::GetFlag(FLAGS6502 f)
{
	return ((status & f) > 0) ? 1 : 0;
}

// Sets or clears a specific bit of the status register
void Processor::SetFlag(FLAGS6502 f, bool v)
{
	if (v)
		status |= f;
	else
		status &= ~f;
}


// Addressing Modes

uint8_t Processor::IMP(){
  fetched = a;
  return 0;
}

uint8_t Processor::IMM(){
  addr_abs = pc++;
  return 0;
}

uint8_t Processor::ZP0(){
  addr_abs = read(pc);
  pc++;
  addr_abs &= 0x00FF;
  return 0;
}

uint8_t Processor::ZPX(){
  addr_abs = (read(pc) + x);
  pc++;
  addr_abs &= 0x00FF;
  return 0;
}

uint8_t Processor::ZPY(){
  addr_abs = (read(pc) + y);
  pc++;
  addr_abs &= 0x00FF;
  return 0;
}

uint8_t Processor::ABS(){

  uint16_t lo = read(pc);
  pc++;

  uint16_t hi = read(pc);
  pc++;

  addr_abs = (hi << 8) | lo;

  return 0;
}

uint8_t Processor::ABX(){

  uint16_t lo = read(pc);
  pc++;
  
  uint16_t hi = read(pc);
  pc++;

  addr_abs = (hi << 8) | lo;

  addr_abs += x;

  if((addr_abs & 0xFF00) != (hi << 8))
    return 1;
  else
    return 0;
}


uint8_t Processor::ABY(){

  uint16_t lo = read(pc);
  pc++;
  
  uint16_t hi = read(pc);
  pc++;

  addr_abs = (hi << 8) | lo;

  addr_abs += y;

  if((addr_abs & 0xFF00) != (hi << 8))
    return 1;
  else
    return 0;
}

uint8_t Processor::IND() 
{
  uint16_t ptr_lo = read(pc);
  pc++;

  uint16_t ptr_hi = read(pc);
  pc++;

  uint16_t ptr = (ptr_hi << 8) | ptr_lo;

  if (ptr_lo == 0x00FF) // Simulate page boundary hardware bug
  {
    addr_abs = (read(ptr & 0xFF00) << 8) | read(ptr + 0);
  }
  else // Behave normaly
   {
     addr_abs = (read(ptr + 1) << 8) | read(ptr + 0);
   }

  return 0;
}

uint8_t Processor::IZX() {

  uint16_t t = read(pc);
  pc++;

  uint16_t lo = read((uint16_t)(t + (uint16_t)x) & 0x00FF);
  uint16_t hi = read((uint16_t)(t + (uint16_t)x + 1) & 0x00FF);

  addr_abs = (hi << 8) | lo;

  return 0;
}

uint8_t Processor::IZY() {

  uint16_t t = read(pc);
  pc++;

  uint16_t lo = read(t & 0x00FF);
  uint16_t hi = read((t + 1) & 0x00FF);

  addr_abs = (hi << 8) | lo;
  addr_abs += y;

  if((addr_abs & 0xFF00) != (hi << 8))
    return 1;
  else
    return 0;

  return 0;
}

uint8_t Processor::REL ()
{
  addr_rel = read(pc);
  pc++;

  if(addr_rel & 0x80)
    addr_rel |= 0xFF00;

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
uint8_t Processor::fetch()
{
	if (!(lookup[opcode].addrmode == &Processor::IMP))
		fetched = read(addr_abs);
	return fetched;
}



///////////////////////////////////////////////////////////////////////////////
// INSTRUCTION IMPLEMENTATIONS

// Note: Ive started with the two most complicated instructions to emulate, which
// ironically is addition and subtraction! Ive tried to include a detailed 
// explanation as to why they are so complex, yet so fundamental. Im also NOT
// going to do this through the explanation of 1 and 2's complement.

// Instruction: Add with Carry In
// Function:    A = A + M + C
// Flags Out:   C, V, N, Z
//
// Explanation:
// The purpose of this function is to add a value to the accumulator and a carry bit. If
// the result is > 255 there is an overflow setting the carry bit. Ths allows you to
// chain together ADC instructions to add numbers larger than 8-bits. This in itself is
// simple, however the 6502 supports the concepts of Negativity/Positivity and Signed Overflow.
//
// 10000100 = 128 + 4 = 132 in normal circumstances, we know this as unsigned and it allows
// us to represent numbers between 0 and 255 (given 8 bits). The 6502 can also interpret 
// this word as something else if we assume those 8 bits represent the range -128 to +127,
// i.e. it has become signed.
//
// Since 132 > 127, it effectively wraps around, through -128, to -124. This wraparound is
// called overflow, and this is a useful to know as it indicates that the calculation has
// gone outside the permissable range, and therefore no longer makes numeric sense.
//
// Note the implementation of ADD is the same in binary, this is just about how the numbers
// are represented, so the word 10000100 can be both -124 and 132 depending upon the 
// context the programming is using it in. We can prove this!
//
//  10000100 =  132  or  -124
// +00010001 = + 17      + 17
//  ========    ===       ===     See, both are valid additions, but our interpretation of
//  10010101 =  149  or  -107     the context changes the value, not the hardware!
//
// In principle under the -128 to 127 range:
// 10000000 = -128, 11111111 = -1, 00000000 = 0, 00000000 = +1, 01111111 = +127
// therefore negative numbers have the most significant set, positive numbers do not
//
// To assist us, the 6502 can set the overflow flag, if the result of the addition has
// wrapped around. V <- ~(A^M) & A^(A+M+C) :D lol, let's work out why!
//
// Let's suppose we have A = 30, M = 10 and C = 0
//          A = 30 = 00011110
//          M = 10 = 00001010+
//     RESULT = 40 = 00101000
//
// Here we have not gone out of range. The resulting significant bit has not changed.
// So let's make a truth table to understand when overflow has occurred. Here I take
// the MSB of each component, where R is RESULT.
//
// A  M  R | V | A^R | A^M |~(A^M) | 
// 0  0  0 | 0 |  0  |  0  |   1   |
// 0  0  1 | 1 |  1  |  0  |   1   |
// 0  1  0 | 0 |  0  |  1  |   0   |
// 0  1  1 | 0 |  1  |  1  |   0   |  so V = ~(A^M) & (A^R)
// 1  0  0 | 0 |  1  |  1  |   0   |
// 1  0  1 | 0 |  0  |  1  |   0   |
// 1  1  0 | 1 |  1  |  0  |   1   |
// 1  1  1 | 0 |  0  |  0  |   1   |
//
// We can see how the above equation calculates V, based on A, M and R. V was chosen
// based on the following hypothesis:
//       Positive Number + Positive Number = Negative Result -> Overflow
//       Negative Number + Negative Number = Positive Result -> Overflow
//       Positive Number + Negative Number = Either Result -> Cannot Overflow
//       Positive Number + Positive Number = Positive Result -> OK! No Overflow
//       Negative Number + Negative Number = Negative Result -> OK! NO Overflow

uint8_t Processor::ADC()
{
	// Grab the data that we are adding to the accumulator
	fetch();
	
	// Add is performed in 16-bit domain for emulation to capture any
	// carry bit, which will exist in bit 8 of the 16-bit word
	temp = (uint16_t)a + (uint16_t)fetched + (uint16_t)GetFlag(C);
	
	// The carry flag out exists in the high byte bit 0
	SetFlag(C, temp > 255);
	
	// The Zero flag is set if the result is 0
	SetFlag(Z, (temp & 0x00FF) == 0);
	
	// The signed Overflow flag is set based on all that up there! :D
	SetFlag(V, (~((uint16_t)a ^ (uint16_t)fetched) & ((uint16_t)a ^ (uint16_t)temp)) & 0x0080);
	
	// The negative flag is set to the most significant bit of the result
	SetFlag(N, temp & 0x80);
	
	// Load the result into the accumulator (it's 8-bit dont forget!)
	a = temp & 0x00FF;
	
	// This instruction has the potential to require an additional clock cycle
	return 1;
}


// Instruction: Subtraction with Borrow In
// Function:    A = A - M - (1 - C)
// Flags Out:   C, V, N, Z
//
// Explanation:
// Given the explanation for ADC above, we can reorganise our data
// to use the same computation for addition, for subtraction by multiplying
// the data by -1, i.e. make it negative
//
// A = A - M - (1 - C)  ->  A = A + -1 * (M - (1 - C))  ->  A = A + (-M + 1 + C)
//
// To make a signed positive number negative, we can invert the bits and add 1
// (OK, I lied, a little bit of 1 and 2s complement :P)
//
//  5 = 00000101
// -5 = 11111010 + 00000001 = 11111011 (or 251 in our 0 to 255 range)
//
// The range is actually unimportant, because if I take the value 15, and add 251
// to it, given we wrap around at 256, the result is 10, so it has effectively 
// subtracted 5, which was the original intention. (15 + 251) % 256 = 10
//
// Note that the equation above used (1-C), but this got converted to + 1 + C.
// This means we already have the +1, so all we need to do is invert the bits
// of M, the data(!) therfore we can simply add, exactly the same way we did 
// before.

uint8_t Processor::SBC()
{
	fetch();
	
	// Operating in 16-bit domain to capture carry out
	
	// We can invert the bottom 8 bits with bitwise xor
	uint16_t value = ((uint16_t)fetched) ^ 0x00FF;
	
	// Notice this is exactly the same as addition from here!
	temp = (uint16_t)a + value + (uint16_t)GetFlag(C);
	SetFlag(C, temp & 0xFF00);
	SetFlag(Z, ((temp & 0x00FF) == 0));
	SetFlag(V, (temp ^ (uint16_t)a) & (temp ^ value) & 0x0080);
	SetFlag(N, temp & 0x0080);
	a = temp & 0x00FF;
	return 1;
}

// OK! Complicated operations are done! the following are much simpler
// and conventional. The typical order of events is:
// 1) Fetch the data you are working with
// 2) Perform calculation
// 3) Store the result in desired place
// 4) Set Flags of the status register
// 5) Return if instruction has potential to require additional 
//    clock cycle


// Instruction: Bitwise Logic AND
// Function:    A = A & M
// Flags Out:   N, Z
uint8_t Processor::AND()
{
	fetch();
	a = a & fetched;
	SetFlag(Z, a == 0x00);
	SetFlag(N, a & 0x80);
	return 1;
}


// Instruction: Arithmetic Shift Left
// Function:    A = C <- (A << 1) <- 0
// Flags Out:   N, Z, C
uint8_t Processor::ASL()
{
	fetch();
	temp = (uint16_t)fetched << 1;
	SetFlag(C, (temp & 0xFF00) > 0);
	SetFlag(Z, (temp & 0x00FF) == 0x00);
	SetFlag(N, temp & 0x80);
	if (lookup[opcode].addrmode == &Processor::IMP)
		a = temp & 0x00FF;
	else
		write(addr_abs, temp & 0x00FF);
	return 0;
}


// Instruction: Branch if Carry Clear
// Function:    if(C == 0) pc = address 
uint8_t Processor::BCC()
{
	if (GetFlag(C) == 0)
	{
		cycles++;
		addr_abs = pc + addr_rel;
		
		if((addr_abs & 0xFF00) != (pc & 0xFF00))
			cycles++;
		
		pc = addr_abs;
	}
	return 0;
}


// Instruction: Branch if Carry Set
// Function:    if(C == 1) pc = address
uint8_t Processor::BCS()
{
	if (GetFlag(C) == 1)
	{
		cycles++;
		addr_abs = pc + addr_rel;

		if ((addr_abs & 0xFF00) != (pc & 0xFF00))
			cycles++;

		pc = addr_abs;
	}
	return 0;
}


// Instruction: Branch if Equal
// Function:    if(Z == 1) pc = address
uint8_t Processor::BEQ()
{
	if (GetFlag(Z) == 1)
	{
		cycles++;
		addr_abs = pc + addr_rel;

		if ((addr_abs & 0xFF00) != (pc & 0xFF00))
			cycles++;

		pc = addr_abs;
	}
	return 0;
}

uint8_t Processor::BIT()
{
	fetch();
	temp = a & fetched;
	SetFlag(Z, (temp & 0x00FF) == 0x00);
	SetFlag(N, fetched & (1 << 7));
	SetFlag(V, fetched & (1 << 6));
	return 0;
}


// Instruction: Branch if Negative
// Function:    if(N == 1) pc = address
uint8_t Processor::BMI()
{
	if (GetFlag(N) == 1)
	{
		cycles++;
		addr_abs = pc + addr_rel;

		if ((addr_abs & 0xFF00) != (pc & 0xFF00))
			cycles++;

		pc = addr_abs;
	}
	return 0;
}


// Instruction: Branch if Not Equal
// Function:    if(Z == 0) pc = address
uint8_t Processor::BNE()
{
	if (GetFlag(Z) == 0)
	{
		cycles++;
		addr_abs = pc + addr_rel;

		if ((addr_abs & 0xFF00) != (pc & 0xFF00))
			cycles++;

		pc = addr_abs;
	}
	return 0;
}


// Instruction: Branch if Positive
// Function:    if(N == 0) pc = address
uint8_t Processor::BPL()
{
	if (GetFlag(N) == 0)
	{
		cycles++;
		addr_abs = pc + addr_rel;

		if ((addr_abs & 0xFF00) != (pc & 0xFF00))
			cycles++;

		pc = addr_abs;
	}
	return 0;
}

// Instruction: Break
// Function:    Program Sourced Interrupt
uint8_t Processor::BRK()
{
	pc++;
	
	SetFlag(I, 1);
	write(0x0100 + stkp, (pc >> 8) & 0x00FF);
	stkp--;
	write(0x0100 + stkp, pc & 0x00FF);
	stkp--;

	SetFlag(B, 1);
	write(0x0100 + stkp, status);
	stkp--;
	SetFlag(B, 0);

	pc = (uint16_t)read(0xFFFE) | ((uint16_t)read(0xFFFF) << 8);
	return 0;
}


// Instruction: Branch if Overflow Clear
// Function:    if(V == 0) pc = address
uint8_t Processor::BVC()
{
	if (GetFlag(V) == 0)
	{
		cycles++;
		addr_abs = pc + addr_rel;

		if ((addr_abs & 0xFF00) != (pc & 0xFF00))
			cycles++;

		pc = addr_abs;
	}
	return 0;
}


// Instruction: Branch if Overflow Set
// Function:    if(V == 1) pc = address
uint8_t Processor::BVS()
{
	if (GetFlag(V) == 1)
	{
		cycles++;
		addr_abs = pc + addr_rel;

		if ((addr_abs & 0xFF00) != (pc & 0xFF00))
			cycles++;

		pc = addr_abs;
	}
	return 0;
}


// Instruction: Clear Carry Flag
// Function:    C = 0
uint8_t Processor::CLC()
{
	SetFlag(C, false);
	return 0;
}


// Instruction: Clear Decimal Flag
// Function:    D = 0
uint8_t Processor::CLD()
{
	SetFlag(D, false);
	return 0;
}


// Instruction: Disable Interrupts / Clear Interrupt Flag
// Function:    I = 0
uint8_t Processor::CLI()
{
	SetFlag(I, false);
	return 0;
}


// Instruction: Clear Overflow Flag
// Function:    V = 0
uint8_t Processor::CLV()
{
	SetFlag(V, false);
	return 0;
}

// Instruction: Compare Accumulator
// Function:    C <- A >= M      Z <- (A - M) == 0
// Flags Out:   N, C, Z
uint8_t Processor::CMP()
{
	fetch();
	temp = (uint16_t)a - (uint16_t)fetched;
	SetFlag(C, a >= fetched);
	SetFlag(Z, (temp & 0x00FF) == 0x0000);
	SetFlag(N, temp & 0x0080);
	return 1;
}


// Instruction: Compare X Register
// Function:    C <- X >= M      Z <- (X - M) == 0
// Flags Out:   N, C, Z
uint8_t Processor::CPX()
{
	fetch();
	temp = (uint16_t)x - (uint16_t)fetched;
	SetFlag(C, x >= fetched);
	SetFlag(Z, (temp & 0x00FF) == 0x0000);
	SetFlag(N, temp & 0x0080);
	return 0;
}


// Instruction: Compare Y Register
// Function:    C <- Y >= M      Z <- (Y - M) == 0
// Flags Out:   N, C, Z
uint8_t Processor::CPY()
{
	fetch();
	temp = (uint16_t)y - (uint16_t)fetched;
	SetFlag(C, y >= fetched);
	SetFlag(Z, (temp & 0x00FF) == 0x0000);
	SetFlag(N, temp & 0x0080);
	return 0;
}


// Instruction: Decrement Value at Memory Location
// Function:    M = M - 1
// Flags Out:   N, Z
uint8_t Processor::DEC()
{
	fetch();
	temp = fetched - 1;
	write(addr_abs, temp & 0x00FF);
	SetFlag(Z, (temp & 0x00FF) == 0x0000);
	SetFlag(N, temp & 0x0080);
	return 0;
}


// Instruction: Decrement X Register
// Function:    X = X - 1
// Flags Out:   N, Z
uint8_t Processor::DEX()
{
	x--;
	SetFlag(Z, x == 0x00);
	SetFlag(N, x & 0x80);
	return 0;
}


// Instruction: Decrement Y Register
// Function:    Y = Y - 1
// Flags Out:   N, Z
uint8_t Processor::DEY()
{
	y--;
	SetFlag(Z, y == 0x00);
	SetFlag(N, y & 0x80);
	return 0;
}


// Instruction: Bitwise Logic XOR
// Function:    A = A xor M
// Flags Out:   N, Z
uint8_t Processor::EOR()
{
	fetch();
	a = a ^ fetched;	
	SetFlag(Z, a == 0x00);
	SetFlag(N, a & 0x80);
	return 1;
}


// Instruction: Increment Value at Memory Location
// Function:    M = M + 1
// Flags Out:   N, Z
uint8_t Processor::INC()
{
	fetch();
	temp = fetched + 1;
	write(addr_abs, temp & 0x00FF);
	SetFlag(Z, (temp & 0x00FF) == 0x0000);
	SetFlag(N, temp & 0x0080);
	return 0;
}


// Instruction: Increment X Register
// Function:    X = X + 1
// Flags Out:   N, Z
uint8_t Processor::INX()
{
	x++;
	SetFlag(Z, x == 0x00);
	SetFlag(N, x & 0x80);
	return 0;
}


// Instruction: Increment Y Register
// Function:    Y = Y + 1
// Flags Out:   N, Z
uint8_t Processor::INY()
{
	y++;
	SetFlag(Z, y == 0x00);
	SetFlag(N, y & 0x80);
	return 0;
}


// Instruction: Jump To Location
// Function:    pc = address
uint8_t Processor::JMP()
{
	pc = addr_abs;
	return 0;
}


// Instruction: Jump To Sub-Routine
// Function:    Push current pc to stack, pc = address
uint8_t Processor::JSR()
{
	pc--;

	write(0x0100 + stkp, (pc >> 8) & 0x00FF);
	stkp--;
	write(0x0100 + stkp, pc & 0x00FF);
	stkp--;

	pc = addr_abs;
	return 0;
}


// Instruction: Load The Accumulator
// Function:    A = M
// Flags Out:   N, Z
uint8_t Processor::LDA()
{
	fetch();
	a = fetched;
	SetFlag(Z, a == 0x00);
	SetFlag(N, a & 0x80);
	return 1;
}


// Instruction: Load The X Register
// Function:    X = M
// Flags Out:   N, Z
uint8_t Processor::LDX()
{
	fetch();
	x = fetched;
	SetFlag(Z, x == 0x00);
	SetFlag(N, x & 0x80);
	return 1;
}


// Instruction: Load The Y Register
// Function:    Y = M
// Flags Out:   N, Z
uint8_t Processor::LDY()
{
	fetch();
	y = fetched;
	SetFlag(Z, y == 0x00);
	SetFlag(N, y & 0x80);
	return 1;
}

uint8_t Processor::LSR()
{
	fetch();
	SetFlag(C, fetched & 0x0001);
	temp = fetched >> 1;	
	SetFlag(Z, (temp & 0x00FF) == 0x0000);
	SetFlag(N, temp & 0x0080);
	if (lookup[opcode].addrmode == &Processor::IMP)
		a = temp & 0x00FF;
	else
		write(addr_abs, temp & 0x00FF);
	return 0;
}

uint8_t Processor::NOP()
{
	// Sadly not all NOPs are equal, Ive added a few here
	// based on https://wiki.nesdev.com/w/index.php/CPU_unofficial_opcodes
	// and will add more based on game compatibility, and ultimately
	// I'd like to cover all illegal opcodes too
	switch (opcode) {
	case 0x1C:
	case 0x3C:
	case 0x5C:
	case 0x7C:
	case 0xDC:
	case 0xFC:
		return 1;
		break;
	}
	return 0;
}


// Instruction: Bitwise Logic OR
// Function:    A = A | M
// Flags Out:   N, Z
uint8_t Processor::ORA()
{
	fetch();
	a = a | fetched;
	SetFlag(Z, a == 0x00);
	SetFlag(N, a & 0x80);
	return 1;
}


// Instruction: Push Accumulator to Stack
// Function:    A -> stack
uint8_t Processor::PHA()
{
	write(0x0100 + stkp, a);
	stkp--;
	return 0;
}


// Instruction: Push Status Register to Stack
// Function:    status -> stack
// Note:        Break flag is set to 1 before push
uint8_t Processor::PHP()
{
	write(0x0100 + stkp, status | B | U);
	SetFlag(B, 0);
	SetFlag(U, 0);
	stkp--;
	return 0;
}


// Instruction: Pop Accumulator off Stack
// Function:    A <- stack
// Flags Out:   N, Z
uint8_t Processor::PLA()
{
	stkp++;
	a = read(0x0100 + stkp);
	SetFlag(Z, a == 0x00);
	SetFlag(N, a & 0x80);
	return 0;
}


// Instruction: Pop Status Register off Stack
// Function:    Status <- stack
uint8_t Processor::PLP()
{
	stkp++;
	status = read(0x0100 + stkp);
	SetFlag(U, 1);
	return 0;
}

uint8_t Processor::ROL()
{
	fetch();
	temp = (uint16_t)(fetched << 1) | GetFlag(C);
	SetFlag(C, temp & 0xFF00);
	SetFlag(Z, (temp & 0x00FF) == 0x0000);
	SetFlag(N, temp & 0x0080);
	if (lookup[opcode].addrmode == &Processor::IMP)
		a = temp & 0x00FF;
	else
		write(addr_abs, temp & 0x00FF);
	return 0;
}

uint8_t Processor::ROR()
{
	fetch();
	temp = (uint16_t)(GetFlag(C) << 7) | (fetched >> 1);
	SetFlag(C, fetched & 0x01);
	SetFlag(Z, (temp & 0x00FF) == 0x00);
	SetFlag(N, temp & 0x0080);
	if (lookup[opcode].addrmode == &Processor::IMP)
		a = temp & 0x00FF;
	else
		write(addr_abs, temp & 0x00FF);
	return 0;
}

uint8_t Processor::RTI()
{
	stkp++;
	status = read(0x0100 + stkp);
	status &= ~B;
	status &= ~U;

	stkp++;
	pc = (uint16_t)read(0x0100 + stkp);
	stkp++;
	pc |= (uint16_t)read(0x0100 + stkp) << 8;
	return 0;
}

uint8_t Processor::RTS()
{
	stkp++;
	pc = (uint16_t)read(0x0100 + stkp);
	stkp++;
	pc |= (uint16_t)read(0x0100 + stkp) << 8;
	
	pc++;
	return 0;
}




// Instruction: Set Carry Flag
// Function:    C = 1
uint8_t Processor::SEC()
{
	SetFlag(C, true);
	return 0;
}


// Instruction: Set Decimal Flag
// Function:    D = 1
uint8_t Processor::SED()
{
	SetFlag(D, true);
	return 0;
}


// Instruction: Set Interrupt Flag / Enable Interrupts
// Function:    I = 1
uint8_t Processor::SEI()
{
	SetFlag(I, true);
	return 0;
}


// Instruction: Store Accumulator at Address
// Function:    M = A
uint8_t Processor::STA()
{
	write(addr_abs, a);
	return 0;
}


// Instruction: Store X Register at Address
// Function:    M = X
uint8_t Processor::STX()
{
	write(addr_abs, x);
	return 0;
}


// Instruction: Store Y Register at Address
// Function:    M = Y
uint8_t Processor::STY()
{
	write(addr_abs, y);
	return 0;
}


// Instruction: Transfer Accumulator to X Register
// Function:    X = A
// Flags Out:   N, Z
uint8_t Processor::TAX()
{
	x = a;
	SetFlag(Z, x == 0x00);
	SetFlag(N, x & 0x80);
	return 0;
}


// Instruction: Transfer Accumulator to Y Register
// Function:    Y = A
// Flags Out:   N, Z
uint8_t Processor::TAY()
{
	y = a;
	SetFlag(Z, y == 0x00);
	SetFlag(N, y & 0x80);
	return 0;
}


// Instruction: Transfer Stack Pointer to X Register
// Function:    X = stack pointer
// Flags Out:   N, Z
uint8_t Processor::TSX()
{
	x = stkp;
	SetFlag(Z, x == 0x00);
	SetFlag(N, x & 0x80);
	return 0;
}


// Instruction: Transfer X Register to Accumulator
// Function:    A = X
// Flags Out:   N, Z
uint8_t Processor::TXA()
{
	a = x;
	SetFlag(Z, a == 0x00);
	SetFlag(N, a & 0x80);
	return 0;
}


// Instruction: Transfer X Register to Stack Pointer
// Function:    stack pointer = X
uint8_t Processor::TXS()
{
	stkp = x;
	return 0;
}


// Instruction: Transfer Y Register to Accumulator
// Function:    A = Y
// Flags Out:   N, Z
uint8_t Processor::TYA()
{
	a = y;
	SetFlag(Z, a == 0x00);
	SetFlag(N, a & 0x80);
	return 0;
}


// This function captures illegal opcodes
uint8_t Processor::XXX()
{
	return 0;
}





///////////////////////////////////////////////////////////////////////////////
// HELPER FUNCTIONS

bool Processor::complete()
{
	return cycles == 0;
}

// This is the disassembly function. Its workings are not required for emulation.
// It is merely a convenience function to turn the binary instruction code into
// human readable form. Its included as part of the emulator because it can take
// advantage of many of the CPUs internal operations to do this.
std::map<uint16_t, std::string> Processor::disassemble(uint16_t nStart, uint16_t nStop)
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
		uint8_t opcode = bus->read(addr, true); addr++;
		sInst += lookup[opcode].name + " ";

		// Get oprands from desired locations, and form the
		// instruction based upon its addressing mode. These
		// routines mimmick the actual fetch routine of the
		// 6502 in order to get accurate data as part of the
		// instruction
		if (lookup[opcode].addrmode == &Processor::IMP)
		{
			sInst += " {IMP}";
		}
		else if (lookup[opcode].addrmode == &Processor::IMM)
		{
			value = bus->read(addr, true); addr++;
			sInst += "#$" + hex(value, 2) + " {IMM}";
		}
		else if (lookup[opcode].addrmode == &Processor::ZP0)
		{
			lo = bus->read(addr, true); addr++;
			hi = 0x00;												
			sInst += "$" + hex(lo, 2) + " {ZP0}";
		}
		else if (lookup[opcode].addrmode == &Processor::ZPX)
		{
			lo = bus->read(addr, true); addr++;
			hi = 0x00;														
			sInst += "$" + hex(lo, 2) + ", X {ZPX}";
		}
		else if (lookup[opcode].addrmode == &Processor::ZPY)
		{
			lo = bus->read(addr, true); addr++;
			hi = 0x00;														
			sInst += "$" + hex(lo, 2) + ", Y {ZPY}";
		}
		else if (lookup[opcode].addrmode == &Processor::IZX)
		{
			lo = bus->read(addr, true); addr++;
			hi = 0x00;								
			sInst += "($" + hex(lo, 2) + ", X) {IZX}";
		}
		else if (lookup[opcode].addrmode == &Processor::IZY)
		{
			lo = bus->read(addr, true); addr++;
			hi = 0x00;								
			sInst += "($" + hex(lo, 2) + "), Y {IZY}";
		}
		else if (lookup[opcode].addrmode == &Processor::ABS)
		{
			lo = bus->read(addr, true); addr++;
			hi = bus->read(addr, true); addr++;
			sInst += "$" + hex((uint16_t)(hi << 8) | lo, 4) + " {ABS}";
		}
		else if (lookup[opcode].addrmode == &Processor::ABX)
		{
			lo = bus->read(addr, true); addr++;
			hi = bus->read(addr, true); addr++;
			sInst += "$" + hex((uint16_t)(hi << 8) | lo, 4) + ", X {ABX}";
		}
		else if (lookup[opcode].addrmode == &Processor::ABY)
		{
			lo = bus->read(addr, true); addr++;
			hi = bus->read(addr, true); addr++;
			sInst += "$" + hex((uint16_t)(hi << 8) | lo, 4) + ", Y {ABY}";
		}
		else if (lookup[opcode].addrmode == &Processor::IND)
		{
			lo = bus->read(addr, true); addr++;
			hi = bus->read(addr, true); addr++;
			sInst += "($" + hex((uint16_t)(hi << 8) | lo, 4) + ") {IND}";
		}
		else if (lookup[opcode].addrmode == &Processor::REL)
		{
			value = bus->read(addr, true); addr++;
			sInst += "$" + hex(value, 2) + " [$" + hex(addr + value, 4) + "] {REL}";
		}

		// Add the formed string to a std::map, using the instruction's
		// address as the key. This makes it convenient to look for later
		// as the instructions are variable in length, so a straight up
		// incremental index is not sufficient.
		mapLines[line_addr] = sInst;
	}

	return mapLines;
}