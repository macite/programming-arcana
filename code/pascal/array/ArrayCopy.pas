/* program: array-copy.c */

#include <string.h>

int main()
{
    int data[3] = {1, 2, 3};
    int other_data[3];
    int more_data[3];
    
    memcpy(other_data, data, 3 * sizeof(int));
    memcpy(more_data, data, sizeof(data));
    
    return 0;
}