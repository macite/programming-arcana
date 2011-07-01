#include <stdio.h>

int main()
{
  printf("Just the format c-string...\n");
  printf("Multi\nLine\nOutput\n");
  printf("1 + 1 = %d\n", 1 + 1);
  printf("Multi values: %d,%4.2f,%s\n", 42, 2.5, "Hello World" );
  printf("Part: %5s - prints Hello\n", "Hello World");
  printf("10\% \\ \n");
  return 0;
}