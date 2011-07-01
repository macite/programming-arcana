/* Program: test-while.c */
#include <stdio.h>
#include <stdbool.h>

bool beg_for_mercy()
{
    char ch;

    printf("Beg for mercy? [y/N]: ");
    scanf("%c", &ch);
    
    return ch == 'y' || ch == 'Y';
}

int main()
{
    bool mercy = false;
    
    printf("Before you stands a 12 foot tall Knight...\n");
    printf("\"We are the Knights who say 'Ni'.\"\n");
    printf("\"I will say Ni to you again if you do not appease us!\"\n");
    
    mercy = beg_for_mercy();
    
    while( !mercy )
    {
        printf("\"Ni!\"\n");
        mercy = beg_for_mercy();
    }
    
    printf("\"Bring us a Shrubbery!\"\n");
    
    return 0;
}