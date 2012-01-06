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

// Draw a rectangle moving across the screen
int main()
{
    int rect_x = 0;
    int rect_y = 250;
    int rect_x_move = MOVE_X;
    
    open_graphics_window("Moving Rectangle", 800, 600);
    load_default_colors();
    
    do
    {
        process_events();
        
        // Update the location of the rectangle
        update_rect_position(rect_x, rect_x_move);
        
        // Clear the screen, then draw the rectangle
        clear_screen(ColorWhite);
        fill_rectangle(ColorRed, rect_x, rect_y, RECT_WIDTH, RECT_HEIGHT);
        draw_framerate(0,0);
        
        // Refresh the screen, keep it at 60fps
        refresh_screen(60);
    } while ( ! window_close_requested() );
    
    return 0;
}