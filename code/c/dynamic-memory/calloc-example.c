#include <stdlib.h>

int main()
{
    int *p, *p_arr;
    
    // get space for one integer from the heap - will be zeroed
    p = (int *)calloc(1, sizeof(int));
    // get space for 5 integers from the heap (like an array) - all zeroed
    p_arr = (int *)malloc(5, sizeof(int));
    
    // free all space allocated
    free(p); 
    p = NULL;
    free(p_arr);
    p_arr = NULL;
    
    return 0;
}