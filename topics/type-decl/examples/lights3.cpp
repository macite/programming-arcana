// Load all of the bitmaps name is based on "size" + "state"
void load_bitmaps()
{
    // Load "on" lights
    load_bitmap_named("small light on", "on_sml.png");
    load_bitmap_named("medium light on", "on_med.png");
    load_bitmap_named("large light on", "on.png");
    
    // Load "off" lights
    load_bitmap_named("small light off", "off_sml.png");
    load_bitmap_named("medium light off", "off_med.png");
    load_bitmap_named("large light off", "off.png");    
}

// ======================
// = Main - Entry Point =
// ======================

int main(int argc, char* argv[])
{
    // Create a number of lights
    light lights[NUM_LIGHTS];
    
    open_audio();
    open_graphics_window("Lights", 800, 600);
    load_default_colors();
    
    load_bitmaps();
    
    // Setup the lights
    lights[0] = create_light(true, SMALL_LIGHT, point_at(10, 10));
    lights[1] = create_light(true, MEDIUM_LIGHT, point_at(110, 10));
    lights[2] = create_light(true, LARGE_LIGHT, point_at(210, 10));
    
    do
    {
        // Update
        process_events();
        update_lights(lights, NUM_LIGHTS);
        
        //Draw
        clear_screen();
        draw_lights(lights, NUM_LIGHTS);
        refresh_screen();
    } while ( ! window_close_requested() );

    release_all_resources();
    close_audio();
    return 0;
}
