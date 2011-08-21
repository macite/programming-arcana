#include <stdio.h>
#include <stdbool.h>
#include "SwinGame.h"

#define BUBBLE_COUNT 10

// Load the bubble image
void load_resources()
{
    load_bitmap_named("bubble", "bubble.png");
}

// Place a bubble somewhere on the screen, 
// and give it a random movement
void place_bubble(sprite bubble)
{
    sprite_set_x(bubble, rnd(screen_width() - sprite_width(bubble)));
    sprite_set_y(bubble, rnd(screen_height() - sprite_height(bubble)));
    sprite_set_dx(bubble, (rnd() * 2) - 1); // between +1 and -1
    sprite_set_dy(bubble, (rnd() * 2) - 1); // between +1 and -1
}

// Create bubbles, and place them on the screen to start
void populate_bubbles(sprite bubbles[], int sz)
{
    int i;
    int bubble_width, bubble_height;
    
    bubble_width = bitmap_width(bitmap_named("bubbles"));
    bubble_height = bitmap_height(bitmap_named("bubbles"));
    
    for (i = 0; i < sz; i++)
    {
        bubbles[i] = create_sprite(bitmap_named("bubble"));
        place_bubble(bubbles[i]);
    }
}

// Update the bubble, move it and check if it is off screen
void update_bubble(sprite bubble)
{
    update_sprite(bubble);  // Moves based on sprites dx,dy
    
    if (sprite_offscreen(bubble)) // is it off screen?
    {
        place_bubble(bubble);   // put it back on screen
    }
}

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
void draw_bubbles(sprite bubbles[], int sz)
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
