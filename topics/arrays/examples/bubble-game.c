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
    // Set the bubble's position
    sprite_set_x(bubble, rnd(screen_width() - sprite_width(bubble)));
    sprite_set_y(bubble, rnd(screen_height() - sprite_height(bubble)));
    
    // Set the bubble's movement
    sprite_set_dx(bubble, (rnd() * 2) - 1); // between +1 and -1
    sprite_set_dy(bubble, (rnd() * 2) - 1); // between +1 and -1
}

// Create bubbles, and place them on the screen to start
void populate_bubbles(sprite bubbles[], int sz)
{
    int i;
    
    for (i = 0; i < sz; i++)    //For each bubble
    {
        bubbles[i] = create_sprite(bitmap_named("bubble")); // Create it
        place_bubble(bubbles[i]); // Place it on the screen
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