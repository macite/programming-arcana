
// Check if the user has performed any actions...
bool process_input(drawing &d)
{
    if(mouse_clicked(LEFT_BUTTON))
    {
        point2d pt = mouse_position();
        
        if(pt.x < MENU_RIGHT_X)
            process_menu_click(d, pt);
        else            
            process_canvas_click(d, pt);
    }
    
    if(any_key_pressed())  
        return process_key(d);
    else
        return false;
}

// ============
// = Main ... =
// ============

int main()
{
    // Create the drawing...
    drawing my_drawing;
    bool quit = false;
    
    // Initialise the drawing with empty data...
    clear_drawing(my_drawing);
    
    open_graphics_window("Draw Shapes", 800, 500);
    load_default_colors();
    
    do
    {
        process_events(); // read user interactions...
        quit = process_input(my_drawing);
        
        draw_drawing(my_drawing);
        refresh_screen();
    } while ( ! window_close_requested() && !quit);
     
    release_all_resources();
    return 0;
}
