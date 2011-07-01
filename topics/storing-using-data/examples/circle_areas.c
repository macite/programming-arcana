/*
* Program: circle_areas.c
* Displays the Circle Areas for circles with radius
* from 1.0 to 5.0 with increments of 0.5.
*/

#include <stdio.h>

#define PI 3.1415

double circle_area(double radius)
{
    return PI * radius * radius;
}

int main()
{
    printf("Circle Areas\n");
    printf("-----------------\n");
    printf(" Radius: 1.0 = %4.2f\n", circle_area(1.0));
    printf(" Radius: 1.5 = %4.2f\n", circle_area(1.5));
    printf(" Radius: 2.0 = %4.2f\n", circle_area(2.0));
    printf(" Radius: 2.5 = %4.2f\n", circle_area(2.5));
    printf(" Radius: 3.0 = %4.2f\n", circle_area(3.0));
    printf(" Radius: 3.5 = %4.2f\n", circle_area(3.5));
    printf(" Radius: 4.0 = %4.2f\n", circle_area(4.0));
    printf(" Radius: 4.5 = %4.2f\n", circle_area(4.5));
    printf(" Radius: 5.0 = %4.2f\n", circle_area(5.0));
    printf("-----------------\n");
    
    return 0;
}