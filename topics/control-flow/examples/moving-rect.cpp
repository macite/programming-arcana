/*
* Moving Rect SwinGame code (C++)
*/
#include <stdio.h>
#include <stdbool.h>
#include "SwinGame.h"

#define RECT_WIDTH 100
#define RECT_HEIGHT 100
#define MOVE_X 5

// Update the x position of the rectangle, by the specified amount
void update_rect_position(int &x, int &dx)
{
    // Move x (passed in by reference)
    x += dx;
    
    // Check if it went of the screen
    if (x < 0)
    {
        // off the left of the screen
        dx = -dx;   // change movement direction
        x = 0;      // put it back on the screen
    }
    else if ((x + RECT_WIDTH) > screen_width())
    {
        // off the screen to the right
        dx = -dx;   // change movement direction
        x = (screen_width() - RECT_WIDTH); // put it back on the screen
    }
}
