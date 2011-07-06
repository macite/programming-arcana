/* Program: test-array.c */
#include <stdio.h>

int main()
{
    int     data[5] = { 0, -1, 2, -3, 4 };
    double  my_data[5];
    float   other[] = {1.2f, 2.5f, 0, -1, 6};
    int i;
    
    for(i = 0; i < 5; i++)
    {
        printf("data[%d]: %d\n", i, data[i]);
        printf("name[%d]: %f\n", i, other[i]);
    }
    return 0;
}