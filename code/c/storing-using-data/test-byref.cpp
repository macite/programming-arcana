/* Program: test-byref.cpp */
#include <stdio.h>

void double_it(int &data)
{
    printf("Data passed in was %d, about to double it...\n", data);
    data = data * 2;
    printf("In double_it data is now %d\n", data);
}

int main()
{
    int val = 3;
    
    printf("In main val is %d\n", val);
    double_it(val);
    printf("Back in main val is now %d\n", val);
}