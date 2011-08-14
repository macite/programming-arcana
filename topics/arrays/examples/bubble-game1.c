// Update all of the bubbles...
void update_bubbles(sprite bubbles[], int sz)
{
    int i;
    
    for (i = 0; i < sz; i++)
    {
        update_bubble(bubbles[i]);
    }
}

// Draw all of the bubbles
void draw_bubbles(const sprite bubbles[], int sz)
{
    int i;
    
    for (i = 0; i < sz; i++)
    {
        draw_sprite(bubbles[i]);
    }
}

// A start of a bubble game...
// Requires "bubble.png" to be placed in Resources/images
int main()
{
    // Create an array of bubbles
    sprite bubbles[BUBBLE_COUNT];
    
    open_audio();
    open_graphics_window("Bubble Pop!", 800, 600);
    load_default_colors();
    
    load_resources();
    populate_bubbles(bubbles, BUBBLE_COUNT);    // Load the bubbles
    
    do
    {
        // Update the game...
        process_events();
        update_bubbles(bubbles, BUBBLE_COUNT);
        
        // Draw the game
        clear_screen();
        
        draw_framerate(0,0);
        draw_bubbles(bubbles, BUBBLE_COUNT);
        
        refresh_screen();
    } while ( ! window_close_requested() );
    
    close_audio();
    
    release_all_resources();
    return 0;
}
