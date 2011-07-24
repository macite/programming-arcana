/*
* Program to signal SOS to the Terminal.
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

// Instructs the computer to perform three short signals,
// signaling the character S
void signal_s()
{
  printf(" ");
  short_signal();
  short_signal();
  short_signal();
}

// Instructs the computer to perform three long signals,
// signaling the character O
void signal_o()
{
  printf(" ");
  long_signal();
  long_signal();
  long_signal();
}

// Entry point, instructs computer to signal SOS in morse code.
int main()
{
  signal_s();
  signal_o();
  signal_s();
  printf("\n");
  
  return 0;
}

