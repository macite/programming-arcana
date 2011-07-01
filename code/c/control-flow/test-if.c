/* Program: test-if.c */

#include <stdio.h>

int main()
{
    int num, num1;
    
    printf("Enter a number: ");
    scanf("%d", &num);
    
    if (num != 2) 
        printf("Num is not 2!\n");
    
    printf("Enter another number: ");
    scanf("%d", &num1);
    
    if (num1 == 2 && num != 2)
        printf("You got the hint... num1 is 2!");
    
    
    if (num > num1)
        printf("The first number you entered was the larger.");
    else
        printf("The first number you entered was not larger.");
    
    return 0;
}