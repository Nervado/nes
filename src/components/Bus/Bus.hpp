#pragma once
#include <cstdint>
#include <array>
#include "../Processor/Processor.hpp"

class Bus
{
private:
  /* data */
public:
  Bus(/* args */);
  ~Bus();
public: // Devices on bus
  Processor cpu;
  std::array<uint8_t, 64 * 1024> ram; // fake ram

public: // bus Read & Write
  void write(uint16_t addr, uint8_t data);
  uint8_t read(uint16_t addr, bool bReadOnly = false);
};

