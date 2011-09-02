#include <stdio.h>

typedef struct 
{
    float x, y;
} point_2d;

int main()
{
    float my_x = 0.0;
    point_2d pt = {1.0,2.0};
    float *f_ptr;
    point_2d *pt_ptr;
    
    f_ptr = &my_x;           // get pointer to my_x variable
    printf("%f\n", *f_ptr);  // print the value pointer to by fptr
    
    f_ptr = &pt.x;
    printf("%f\n", *f_ptr);  // print the value pointer to by fptr
    printf("%f,%f\n", f_ptr[0], f_ptr[1]); // prints pt.x, pt.y (bad practice)
    
    pt_ptr = &pt;
    // follow pointer, and get x and y fields from what it points to...
    printf("%f,%f\n", pt_ptr->x, pt_ptr->y);
    printf("%f,%f\n", (*pt_ptr).x, (*pt_ptr).y);  // same as above
    return 0;
}