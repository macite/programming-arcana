#define LARGE_RADIUS 6
#define SMALL_RADIUS 3

void draw_cursor()
{
    float dot_x, dot_y;
    hide_mouse();
    
    while( ! window_close_requested() )
    {
        process_events();
        
        dot_x = mouse_x();
        dot_y = mouse_y();
        
        clear_screen(ColorWhite);
        draw_framerate(0,0);
        draw_circle(ColorBlack, dot_x, dot_y, LARGE_RADIUS);
        
        if ( mouse_down(LEFT_BUTTON) )
            fill_circle(ColorBlack, dot_x, dot_y, SMALL_RADIUS);
        else
            draw_circle(ColorBlack, dot_x, dot_y, SMALL_RADIUS);
        
        refresh_screen();
    }
    
    show_mouse();
}