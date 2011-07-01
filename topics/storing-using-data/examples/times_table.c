/*
* Program: times_table.c
* Displays the Times Table from 1 x n to 10 x n.
*/

#include <stdio.h>

int main()
{
    int number = 0;
    
    printf("Times Table\n");
    
    printf("Enter number: ");
    scanf("%d", &number);
    
    printf("-----------------\n");
    printf(" 1 x %d = %d\n", number, 1 * number);
    printf(" 2 x %d = %d\n", number, 2 * number);
    printf(" 3 x %d = %d\n", number, 3 * number);
    printf(" 4 x %d = %d\n", number, 4 * number);
    printf(" 5 x %d = %d\n", number, 5 * number);
    printf(" 6 x %d = %d\n", number, 6 * number);
    printf(" 7 x %d = %d\n", number, 7 * number);
    printf(" 8 x %d = %d\n", number, 8 * number);
    printf(" 9 x %d = %d\n", number, 9 * number);
    printf("10 x %d = %d\n", number, 10 * number);
    printf("-----------------\n");
    
    return 0;
}