/* Program: lights (C++, SwinGame) */
#include <stdbool.h>
#include <strings.h>
#include <stdio.h>
#include "SwinGame.h"

// =====================
// = Delcare Constants =
// =====================

#define NUM_LIGHTS 3

// =================
// = Declare Types =
// =================

// There are three sizes of light...
typedef enum
{
    SMALL_LIGHT,
    MEDIUM_LIGHT,
    LARGE_LIGHT
} light_size;

// A light is on/off, have a size, and a position
typedef struct
{
    bool        is_on;      // is the light on?
    light_size  size;       // size of the light
    point2d     position;   // location of the light (top left)
} light;


// ====================================
// = Declare functions and procedures =
// ====================================

// Sets up a new light, and returns its data
light create_light(bool on, light_size sz, point2d pos)
{
    light result;
    
    result.is_on = on;          // sets the light on/off
    result.size = sz;           // sets the size of the light
    result.position = pos;      // sets its position
    
    return result;              // return the initialised light
}

// Get the bitmap to use for the light "l"
bitmap light_bitmap(light &l)
{
    char name[17] = ""; // construct the name of the bitmap... 16+1
    
    // the start of the name is based on the size of the bitmap
    switch (l.size)
    {
        case SMALL_LIGHT:
            strncat(name, "small light", 11);
            break;
        case MEDIUM_LIGHT:
            strncat(name, "medium light", 12);
            break;
        case LARGE_LIGHT:
            strncat(name, "large light", 11);
            break;
        default:
            return NULL;
    }
    
    // the end of the name is based on if the light is on/off
    if (l.is_on)
        strncat(name, " on", 4);
    else
        strncat(name, " off", 4);
    
    // return the bitmap with that name
    return bitmap_named(name);    
}

// Draw the light "l" to the screen
void draw_light(light &l)
{
    draw_bitmap(light_bitmap(l), l.position);
}

// Draw all of the lights in "lights"
void draw_lights(light lights[], int count)
{
    int i;
    
    for(i = 0; i < count; i++)
    {
        draw_light(lights[i]);
    }
}

// Is the light currently under the mouse?
bool light_under_mouse(light &l)
{
    point2d mouse;
    bitmap light_bmp;
    
    // get the mouse position
    mouse = mouse_position();
    // get the light bitmap, to determine its size etc.
    light_bmp = light_bitmap(l);
    
    // Simple version using a bounded rectangle
    //return point_in_rect(mouse, bitmap_rectangle(l.position.x, l.position.y, light_bmp));
    return bitmap_point_collision(light_bmp, l.position.x, l.position.y, mouse);
}

// Check if the lights have been changed (clicked)
void update_lights(light lights[], int count)
{
    int i;
    
    // only change if the mouse was clicked
    if ( mouse_clicked(LEFT_BUTTON) )
    {
        // for all of the lights
        for (i = 0; i < count; i++)
        {
            // if the light is under the mouse
            if (light_under_mouse(lights[i]))
            {
                // change state (on = off, off = on)
                lights[i].is_on = ! lights[i].is_on;
            }
        }
    }
}

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
