/* Program: Draw Sun Scene */
#include "SwinGame.h"

void draw_sun()
{
    //Draw the sun
    fill_circle(ColorYellow, 50.0, 50.0, 20);
    draw_circle(ColorRed, 50.0, 50.0, 15);
    draw_circle(ColorRed, 50.0, 50.0, 12);
    draw_circle(ColorRed, 50.0, 50.0, 9);
    draw_circle(ColorRed, 50.0, 50.0, 6);
    fill_circle(ColorWhite, 50.0, 50.0, 3);
    draw_line(ColorYellow, 50, 75, 50, 110);
    draw_line(ColorYellow, 25, 75, 0, 100);
    draw_line(ColorYellow, 75, 75, 100, 100);
    draw_line(ColorYellow, 0, 50, 25, 50);
    draw_line(ColorYellow, 75, 50, 100, 50);
    draw_line(ColorYellow, 25, 25, 0, 0);
    draw_line(ColorYellow, 75, 25, 100, 0);
    draw_line(ColorYellow, 50, 0, 50, 25);
}

int main()
{
    open_graphics_window("Draw Sun", 800, 600);
    load_default_colors();
    
    clear_screen_to(ColorBlue);
    draw_sun();
    
    refresh_screen();
    delay(5000);
    
    release_all_resources();
    return 0;
}
