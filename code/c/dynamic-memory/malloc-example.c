#include <stdlib.h>

int main()
{
    int *p, *p_arr;
    
    // get space for one integer from the heap
    p = (int *)malloc(sizeof(int));
    // get space for 5 integers from the heap (like an array)
    p_arr = (int *)malloc(sizeof(int) * 5);
    
    // free all space allocated
    free(p); 
    p = NULL;
    free(p_arr);
    p_arr = NULL;
    
    return 0;
}