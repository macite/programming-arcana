#include <stdbool.h>
#include "SwinGame.h"

#define BTN_X 100
#define BTN_Y 100
#define BTN_W 200
#define BTN_H 200

bool mouse_over(int x, int y, int width, int height)
{
    float mx, my;
    
    mx = mouse_x();
    my = mouse_y();
    
    return mx >= x && mx <= x + width && my >= y && my <= y + height;
}

bool button_clicked(int x, int y, int width, int height)
{
    return mouse_clicked(LEFT_BUTTON) && mouse_over(x, y, width, height);
}

// Draw a rectangle moving across the screen
int main()
{
    open_graphics_window("Moving Rectangle", 800, 600);
    load_default_colors();
    
    do
    {
        process_events();
        
        // Clear the screen, then draw the "button"
        clear_screen();
        
        if ( mouse_down(LEFT_BUTTON) && mouse_over(BTN_X, BTN_Y, BTN_W, BTN_H) )
            fill_rectangle(ColorBlue, BTN_X, BTN_Y, BTN_W, BTN_H);
        else
            draw_rectangle(ColorBlue, BTN_X, BTN_Y, BTN_W, BTN_H);
        
        if (button_clicked(BTN_X, BTN_Y, BTN_W, BTN_H))
        {
            draw_text("CLICKED", ColorBlue, 0, 20);
        }
        
        draw_framerate(0,0);
        
        // Refresh the screen, keep it at 60fps
        refresh_screen(60);
    } while ( ! window_close_requested() );
    
    release_all_resources();
    return 0;
}

