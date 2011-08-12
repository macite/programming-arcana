/*
* Program: times_table.c
* Displays the Times Table from 1 x n to 10 x n.
*/

#include <stdio.h>

int main()
{
    int number = 0;
    int i;
    
    printf("Times Table\n");
    
    printf("Enter number: ");
    scanf("%d", &number);
    
    printf("-----------------\n");
    
    i = 1;
    while(i < 10)
    {
        printf(" %d x %d = %d\n", i, number, i * number);
        i++;
    }
    printf("-----------------\n");
    
    return 0;
}