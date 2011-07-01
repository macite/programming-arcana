/* Program: MorseCalling - main.c
*  Outputs audio for the CQ "Calling Anyone" signal.
*/
#include <stdio.h>
#include "SwinGame.h"

void short_signal()
{
    play_sound_effect("dit");
    delay(200);
}

void long_signal()
{
    play_sound_effect("dah");
    delay(600);
}

void signal_c()
{
    delay(600);
    long_signal();
    short_signal();
    long_signal();
    short_signal();
}

void signal_q()
{
    delay(600);
    long_signal();
    long_signal();
    short_signal();
    long_signal();
}

void load_sounds()
{
    load_sound_effect_named("dah", "dah.wav");
    load_sound_effect_named("dit", "dit.wav");
}

int main()
{
    open_audio();
    
    load_sounds();
    
    signal_c();
    signal_q();
    
    close_audio();
    release_all_resources();
    return 0;
}