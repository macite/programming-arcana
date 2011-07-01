/* Program: simple-case.c */
#include <stdio.h>

int main()
{
    char ch;
    printf("Enter a character: ");
    scanf("%c", &ch);
    
    switch(ch)
    {
        case 'a': 
        case 'b': printf("A or B\n");
                  break;
        case 'c': printf("C ");
        case 'd': printf("and D\n");
        default:  printf("Something else...\n");
    }
    return 0;
}