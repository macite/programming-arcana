/*
* Program: circle_areas.c
* Displays the Circle Areas for circles with radius
* from 1.0 to 5.0 with increments of 0.5.
*/

#include <stdio.h>

int main()
{
    printf("Circle Areas\n");
    printf("-----------------\n");
    printf(" Radius: 1.0 = %4.2f\n", 3.1415 * 1.0 * 1.0);
    printf(" Radius: 1.5 = %4.2f\n", 3.1415 * 1.5 * 1.5);
    printf(" Radius: 2.0 = %4.2f\n", 3.1415 * 2.0 * 2.0);
    printf(" Radius: 2.5 = %4.2f\n", 3.1415 * 2.5 * 2.5);
    printf(" Radius: 3.0 = %4.2f\n", 3.1415 * 3.0 * 3.0);
    printf(" Radius: 3.5 = %4.2f\n", 3.1415 * 3.5 * 3.5);
    printf(" Radius: 4.0 = %4.2f\n", 3.1415 * 4.0 * 4.0);
    printf(" Radius: 4.5 = %4.2f\n", 3.1415 * 4.5 * 4.5);
    printf(" Radius: 5.0 = %4.2f\n", 3.1415 * 5.0 * 5.0);
    printf("-----------------\n");
    
    return 0;
}