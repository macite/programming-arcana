/* Program: test-jump.c */
#include <stdio.h>

int main()
{
    int i = 0;
    char ch;
    
    while(i < 10000000)
    {
        i++;
        
        // Skip all even numbers
        if ( i % 2 == 0 ) continue;
        
        printf("At %d\n", i);
        
        printf("Quit? [y/N]: ");
        scanf(" %c", &ch);
        
        if (ch == 'y' || ch == 'Y')
        {
            printf("Quitting loop...");
            break;
        }
    }
    
    // Ending function
    return 0;
    
    // Code cannot be reached!
    printf("This will never be printed!\n");
}