#include "Bus.h"

Bus::Bus(/* args */)
{
  // Clear RAM contents, just in case :P
  for (auto &i : ram)
    i = 0x00;
}

Bus::~Bus(/* args */)
{
}
