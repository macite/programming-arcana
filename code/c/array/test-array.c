/* Program: test-array.c */

#include <stdio.h>

int main()
{
    char name[] = "Fred";
    int data[3] = { 0, 0, 0 };
    int i;
    
    name[0] = 'f';
    
    for(i = 0; i < 3; i++)
    {
        printf("data[%d]: %d\n", i, data[i]);
        printf("name[%d]: %c %p\n", i, name[i], &name[i]);
    }
    
    
    
    return 0;
}