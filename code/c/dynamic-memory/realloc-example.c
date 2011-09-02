#include <stdlib.h>

int main()
{
    int *p = NULL;
    
    // get space for one integer from the heap
    p = (int *)realloc(p, sizeof(int));
    *p = 1; // give it a value...
    
    // reallocate space for 5 integers
    p = (int *)realloc(p, sizeof(int) * 5);

    // p[0] is still 1... 
    // but there is now also space for p[1]...p[4]
    
    // free all space allocated
    free(p); 
    p = NULL;
    
    return 0;
}