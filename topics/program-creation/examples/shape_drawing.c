/*
* Program: shape_drawing.c
* Draws a number of shapes to the screen using SwinGame.
*/
#include <stdio.h>
#include <stdbool.h>
#include "SwinGame.h"

int main()
{
    open_graphics_window("Shape Drawing", 800, 600);
    load_default_colors();
    
    clear_screen();
    
    fill_rectangle(ColorWhite, 10, 10, 780, 580);
    
    refresh_screen();
    delay(500);
    
    fill_circle(ColorRed, 50, 50, 25);
    fill_circle(ColorGreen, 80, 50, 25);
    fill_circle(ColorBlue, 110, 50, 25);
    
    refresh_screen();
    delay(500);
    
    fill_triangle(ColorYellow, 100, 100, 150, 175, 210, 115);
    refresh_screen();
    delay(2000);
    
    release_all_resources();
    return 0;
}
