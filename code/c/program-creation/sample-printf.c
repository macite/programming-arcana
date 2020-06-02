#include "splashkit.h"

int main()
{
  write_line("Multi\nLine\nOutput\n");
  // outputs then goes to new line
  write_line("1 + 1 = " + to_string(1 + 1));

  write("3 x 2 = "); // outputs, but does not go to a new line due to write
  write_line(3 * 2); // so this will appear on same line as "3 x 2 ="

  return 0;
}