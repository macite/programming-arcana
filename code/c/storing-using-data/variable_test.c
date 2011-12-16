/* Program: variable_test.c
*  This program demonstrates some variable declarations.
*/
#include <stdio.h>

const float PI = 3.1415;
float global_float = 12.3;
int global_int = 73;

void test(int param_int, float param_float)
{
    int my_local = 37, another_local = 42;
    printf("my local int = %d, another_local = %d\n", my_local, another_local);
    printf("param int = %d, param float = %f\n", param_int, param_float);
    printf("globals are %f and %d\n", global_float, global_int);
}

int main()
{
    int local_int;
    local_int = 21;
    
    test(local_int, PI * local_int * local_int);
    
    printf("local int = %d\n", local_int);
    printf("globals are %f and %d\n", global_float, global_int);
    printf("PI is a constant with value %f\n", PI);
    return 0;
}