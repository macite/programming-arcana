/* Program: test-fn-calls.c */
#include <stdio.h>

int square(int x) { return x * x; }
int sum(int a, int b) { return a + b; }

int main()
{
    int answer = sum(square(5), square(4));
    printf("5 squared + 4 squared is %d\n", answer);
    
    printf("(1 + 2) + (3 + 4) = %d\n", sum(sum(1, 2), sum(3, 4)));
    printf("2 squared, squared = %d\n", square(square(2)));
    
    return 0;
}