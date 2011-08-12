// Draw a rectangle moving across the screen
int main()
{
    int rect_x = 0;
    int rect_y = 250;
    int rect_x_move = MOVE_X;
    
    open_audio();
    open_graphics_window("Moving Rectangle", 800, 600);
    load_default_colors();
    
    do
    {
        process_events();
        
        // Update the location of the rectangle
        update_rect_position(rect_x, rect_x_move);
        
        // Clear the screen, then draw the rectangle
        clear_screen();
        fill_rectangle(ColorRed, rect_x, rect_y, RECT_WIDTH, RECT_HEIGHT);
        draw_framerate(0,0);
        
        // Refresh the screen, keep it at 60fps
        refresh_screen(60);
    } while ( ! window_close_requested() );
    
    close_audio();
    
    release_all_resources();
    return 0;
}
