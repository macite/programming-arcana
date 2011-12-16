/* 
*  Program: output_test.c
*  Writes some messages to the Terminal.
*/
#include <stdio.h>

int main()
{
  // Output the messages...
  printf("Output Test Program\n");
  printf(" 1 + 1 = %d\n", 1 + 1);
  printf(" Area of a circle with radius 3 = %.2f\n", 3.1415 * 3 * 3);
  
  // Finish
  return 0;
}