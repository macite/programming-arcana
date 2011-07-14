/* program: test-calloc.c */
#include <stdlib.h>
#include <stdio.h>

int main()
{
    int *p;
    int i, sz;
    
    printf("How many values do you want allocated: ");
    scanf("%d", &sz);
        
    p = (int*)calloc(sz, sizeof(int));
    
    for (i = 0; i < sz; i++)
    {
        p[i] = 10 * i;
    }
    
    for (i = 0; i < sz; i++)
    {
        printf("p[%d] is at %p and has value %d\n", i, &p[i], p[i]);
    }
    
    free(p);
    p = NULL;
    
    return 0;
}