/* Program: test-return.c */
#include <stdio.h>

int test_return()
{
    printf("test-return started\n");
    return 3;
    printf("Cannot be run as code returned above!");
}

int main()
{
    printf("Calling test_return - the value %d is returned!\n", test_return());
    return 0;
}