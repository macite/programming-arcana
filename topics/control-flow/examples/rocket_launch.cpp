/* Program: RocketLaunch/src/main.c, SwinGame */
#include <stdio.h>
#include <stdbool.h>
#include "SwinGame.h"

// ====================
// = Define constants =
// ====================
#define ROCKET_WIDTH 50
#define ROCKET_HEIGHT 200
#define ROCKET_NOSE_PROPORTION 0.15
#define ROCKET_FIN_Y_PROPORTION 0.4
#define ROCKET_FIN_X_PROPORTION 0.4
#define EXHAUST_LENGTH ROCKET_HEIGHT * 0.25

// ==============
// = Procedures =
// ==============
// Draw the rocket on to the screen with the given colour
void draw_rocket(color rocket_color, float x, float y, bool thrusters_on)
{
    float rocket_mid = x + (ROCKET_WIDTH * 0.5);
    float nose_height = ROCKET_HEIGHT * ROCKET_NOSE_PROPORTION;

    //triangle for the nosecone
    fill_triangle(rocket_color, rocket_mid, y,
        x, y + nose_height,
        x + ROCKET_WIDTH - 1, y + nose_height);
    //rectangle for the rocket body
    fill_rectangle(rocket_color, x, y + nose_height, 
        ROCKET_WIDTH, ROCKET_HEIGHT - nose_height);
    //triangle for both fins (most of it will be behind the body)
    fill_triangle(rocket_color, 
        rocket_mid, y + ROCKET_FIN_Y_PROPORTION * ROCKET_HEIGHT,
        x - ROCKET_WIDTH * ROCKET_FIN_X_PROPORTION, y + ROCKET_HEIGHT,
        x + ROCKET_WIDTH + (ROCKET_WIDTH * ROCKET_FIN_X_PROPORTION), 
        y + ROCKET_HEIGHT);

    if(thrusters_on)
    {
        float big_flame_offset = (rocket_mid - x) * 0.1;
        float little_flame_offset = (rocket_mid - x) * 0.6;
        //big flame
        fill_triangle(rgbcolor(255, 255, 0), 
            x + big_flame_offset, y + ROCKET_HEIGHT,
            x + ROCKET_WIDTH - big_flame_offset, y + ROCKET_HEIGHT,
            rocket_mid, y + ROCKET_HEIGHT + EXHAUST_LENGTH);
        //little flame (inside/on top of the big flame)
        fill_triangle(rgbcolor(255, 180, 0), 
            x + little_flame_offset, y + ROCKET_HEIGHT,
            x + ROCKET_WIDTH - little_flame_offset, y + ROCKET_HEIGHT,
            rocket_mid, y + ROCKET_HEIGHT + EXHAUST_LENGTH * 0.5);
    }
}