#include <iostream>
#include "./Components/Processor/Processor.h"
#include "./Components/Bus/Bus.h"

using namespace std;

int main()
{
  
  Bus bus;
  Processor p;
  printf("Hello Nes\n");

  int i = 0;

  cout << "1 << " << i << " = " << (1 << i) << endl;

  i = 1;

  cout << "1 << " << i << " = " << (1 << i) << endl;

  i = 2;

  cout << "1 << " << i << " = " << (1 << i) << endl;
}