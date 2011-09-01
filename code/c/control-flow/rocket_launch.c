/* Program: RocketLaunch/src/main.c, SwinGame */

#include <stdio.h>
#include <stdbool.h>
#include "SwinGame.h"

// ====================
// = Define constants =
// ====================

#define ROCKET_WIDTH 50
#define ROCKET_HEIGHT 200
#define ROCKET_NOSE_PROPORTION 0.15
#define ROCKET_FIN_Y_PROPORTION 0.4
#define ROCKET_FIN_X_PROPORTION 0.4
#define EXHAUST_LENGTH ROCKET_HEIGHT * 0.25


// ==============
// = Procedures =
// ==============

// Draw the rocket on to the screen with the given colour
void draw_rocket(color rocket_color, float x, float y, bool thrusters_on)
{
    float rocket_mid = x + (ROCKET_WIDTH * 0.5);
    float nose_height = ROCKET_HEIGHT * ROCKET_NOSE_PROPORTION;

    //triangle for the nosecone
    fill_triangle(rocket_color, rocket_mid, y,
        x, y + nose_height,
        x + ROCKET_WIDTH - 1, y + nose_height);
    //rectangle for the rocket body
    fill_rectangle(rocket_color, x, y + nose_height, ROCKET_WIDTH, ROCKET_HEIGHT - nose_height);
    //triangle for both fins (most of it will be behind the body)
    fill_triangle(rocket_color, rocket_mid, y + ROCKET_FIN_Y_PROPORTION * ROCKET_HEIGHT,
        x - ROCKET_WIDTH * ROCKET_FIN_X_PROPORTION, y + ROCKET_HEIGHT,
        x + ROCKET_WIDTH + (ROCKET_WIDTH * ROCKET_FIN_X_PROPORTION), y + ROCKET_HEIGHT);

    if(thrusters_on)
    {
        float big_flame_offset = (rocket_mid - x) * 0.1;
        float little_flame_offset = (rocket_mid - x) * 0.6;
        //big flame
        fill_triangle(rgbcolor(255, 255, 0), x + big_flame_offset, y + ROCKET_HEIGHT,
            x + ROCKET_WIDTH - big_flame_offset, y + ROCKET_HEIGHT,
            rocket_mid, y + ROCKET_HEIGHT + EXHAUST_LENGTH);
        //little flame (inside/on top of the big flame)
        fill_triangle(rgbcolor(255, 180, 0), x + little_flame_offset, y + ROCKET_HEIGHT,
            x + ROCKET_WIDTH - little_flame_offset, y + ROCKET_HEIGHT,
            rocket_mid, y + ROCKET_HEIGHT + EXHAUST_LENGTH * 0.5);
    }
}

//Update the position and thruster values for the rocket, based on user input
void update_rocket(int &rocket_y, bool &thrusters_on)
{
    // Get the loaded sound effect once... then use the variable
    sound_effect launch_sound = sound_effect_named("thrusters");
    
    //is spacebar being pressed now?
    if(key_down(VK_SPACE))
    {
        //move the rocket up a bit and set the thrusters to on
        rocket_y -= 1;
        thrusters_on = true;
        if(!sound_effect_playing(launch_sound))
        {
            play_sound_effect(launch_sound);
        }
    }
    else
    {
        //move the rocket down a bit (gravity) and turn thrusters off
        rocket_y += 1;
        thrusters_on = false;
        if(sound_effect_playing(launch_sound))
        {
            stop_sound_effect(launch_sound);
        }
    }
    
    //make sure the rocket doesn't fall off the bottom of the screen
    //if its y position is too big
    if(rocket_y > screen_height() - ROCKET_HEIGHT)
    {
        //reset it to the maximum value
        rocket_y = screen_height() - ROCKET_HEIGHT;
    }
}

//load the thruster sound effect
void load_resources()
{
    load_sound_effect_named("thrusters", "thrusters.wav");
}

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
        update_rocket(rocket_y, thrusters_on);  // parameters passed by reference (C++)
        
        //draw the rocket on the screen!
        clear_screen();
        draw_rocket(rocket_color, rocket_x, rocket_y, thrusters_on);
        refresh_screen();
    }
    
    close_audio();
    
    release_all_resources();
    return 0;
}
