/*
* Program: morse-calling.c
*
* Outputs the morse code for the signal "Calling Anyone" (cq)
*/

#include <stdio.h>

// Outputs a dot for a short signal
void short_signal()
{
  printf(".");
}

// Outputs a dash for a long signal
void long_signal()
{
  printf("-");
}

void signal_c()
{
  printf(" ");
  long_signal();
  short_signal();
  long_signal();
  short_signal();
}

void signal_q()
{
  printf(" ");
  long_signal();
  long_signal();
  short_signal();
  long_signal();
}

int main()
{
  signal_c();
  signal_q();
  printf("\n");
  
  return 0;
}