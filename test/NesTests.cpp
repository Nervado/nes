#include <gtest/gtest.h>
#include "../src/Components/Bus/Bus.h"

TEST(ExampleTests__Processor_Bus, Bus)
{
  Bus p;
  int b;
  b = 1;
  EXPECT_EQ(1, b);
  EXPECT_FALSE(false);
}
