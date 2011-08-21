#include <stdio.h>
#include <stdbool.h>
#include "SwinGame.h"

// Constant values
#define ECCENTRICITY 0.995f
#define DISTANCE_POLE_DIRECTRIX 1.828f
#define SUN_X 10
#define SUN_Y 300
#define SUN_RADIUS 2
#define COMET_RADIUS 1
#define MAX_DISTANCE 380.0f

#define MIN_WAIT 100
#define MAX_POPORTION_WAIT 900

// Calculated constants
const float SCREEN_SCALE_FACTOR = 800.0f / MAX_DISTANCE;

// Calculate the "r" (radius) of the comets position at a given angle
// using the orbit equation r = ed / (1 + e sin(angle) )
float hale_bopp_r(float angle)
{
    return ECCENTRICITY * DISTANCE_POLE_DIRECTRIX 
           / (1 + ECCENTRICITY * sine(angle));
}

// Calculate the "x" position on the screen from the "y" position of the commet
// the comets orbit will give large values for "y", so it would be better to plot
// these across the screen, so this function translates the comet y to a screen x
float comet_y_to_screen_x(float y)
{
    float result;
    
    // Scale to screen coordinates... reverse sign (- to +)
    result = y * -SCREEN_SCALE_FACTOR;
    
    // Translate so 0,0 is at SUN_X,SUNY
    result = result + SUN_X;
    
    return result;
}

// As with comet_y_to_screen_x, except it calculates the screen y from the comet x
float comet_x_to_screen_y(float x)
{
    float result;
    
    // Scale to screen coordinates
    result = x * SCREEN_SCALE_FACTOR;
    
    // Translate so 0,0 is at SUN_X,SUNY
    result = result + SUN_Y;
    
    return result;
}