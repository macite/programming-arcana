// ======================
// = Main - Entry Point =
// ======================

// Run the rocket launch...
int main()
{
    //declare variables
    int rocket_x, rocket_y;
    bool thrusters_on = false;
    color rocket_color;
    
    open_audio();
    open_graphics_window("Rocket Launch", 800, 600);
    load_default_colors();
    
    //initialise the rocket position, color
    rocket_color = ColorBlue;
    rocket_y = screen_height() - ROCKET_HEIGHT;
    rocket_x = (screen_width() - ROCKET_WIDTH) / 2;
    
    load_resources();
    
    while(!window_close_requested() && !key_typed(VK_ESCAPE))
    {
        //let swingame process user input
        process_events();
        
        //Update the rocket
        update_rocket(rocket_y, thrusters_on);  // passed by reference (C++)
        
        //draw the rocket on the screen!
        clear_screen();
        draw_rocket(rocket_color, rocket_x, rocket_y, thrusters_on);
        refresh_screen();
    }
    
    close_audio();
    
    release_all_resources();
    return 0;
}
