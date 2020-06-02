/* 
*  Program: output_test.c
*  Writes some messages to the Terminal.
*/
#include "splashkit.h"

int main()
{
  // Output the messages...
  write_line("Output Test Program");
  write_line(" 1 + 1 = " + to_string(1 + 1));
  write_line(" Area of a circle with radius 3 = " + to_string(3.1415 * 3 * 3) );
  
  // Finish
  return 0;
}