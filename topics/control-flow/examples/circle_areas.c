/*
* Program: circle_areas.c
* Displays the Circle Areas for circles with radius
* from 1.0 to 5.0 with increments of 0.1.
*/

#include <stdio.h>

#define PI 3.1415

#define START_RADIUS    1.0
#define END_RADIUS      5.0
#define RADIUS_INC      0.1

double circle_area(double radius)
{
    return PI * radius * radius;
}

int main()
{
    double radius; 
    
    printf("Circle Areas\n");
    printf("-----------------\n");
    
    radius = START_RADIUS;
    
    while (radius <= END_RADIUS)
    {
        printf(" Radius: %4.2f = %4.2f\n", radius, circle_area(radius));
        radius = radius + RADIUS_INC;
    }
    
    printf("-----------------\n");
    
    return 0;
}