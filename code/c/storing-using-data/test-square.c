/* Program: test-square.c */
#include <stdio.h>

int square(int val)
{
    return val * val;
}

int main()
{
    printf("5 squared is %d\n", square(5));
    return 0;
}