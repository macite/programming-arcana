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