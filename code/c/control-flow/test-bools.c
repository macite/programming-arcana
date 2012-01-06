/* Program: test-bools.c */
#include <stdio.h>
#include <stdbool.h>

void print_bool(bool value)
{
    if(value) 
        printf("true");
    else 
        printf("false");
}

// Is v1 at least double v2
bool at_least_double(int v1, int v2)
{
    return v1 >= 2 * v2;
}

int main()
{
    bool test;
    int num;
    
    printf("Enter a number: ");
    scanf("%d", &num);
    
    test = at_least_double(num, 5);
    printf("%d is at least double 5 is ", num);
    print_bool(test);
    printf("\n");
    
    return 0;
}