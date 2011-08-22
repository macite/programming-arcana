/* Program: bike-race.c, SwinGame */

#include <stdio.h>
#include <stdbool.h>
#include "SwinGame.h"

// ====================
// = Define constants =
// ====================

#define WHEEL_SIZE 10
#define WHEEL_GAP 10
#define SEAT_GAP 5
#define MAX_ACCELERATION 10
#define RACE_DURATION 30
#define X_SCALE_FACTOR 0.15

// =============
// = Functions =
// =============

// Calculate the distance travelled given acceleration and time
// distance = ut + ((at^2) / 2)
float distance_travelled(float initial_speed, float acceleration, float time)
{
    return (initial_speed * time) + ((acceleration * time * time) / 2);
}

// Calculate the x position of a bike accelerating at the given
// acceleration for the duration of the race 
float bike_x_for_accel(float acceleration)
{
    float distance;
    
    distance = distance_travelled(0, acceleration, RACE_DURATION);
    
    return distance * X_SCALE_FACTOR;
}

// Come up with a random acceleration value for a bike between 0 and 
// MAX_ACCELERATION
float random_accel()
{
    return rnd() * MAX_ACCELERATION;
}

// ==============
// = Procedures =
// ==============

// Draw the bike to the screen in the given color
void draw_bike(color bike_color, float x, float y)
{
    float left_wheel_x, right_wheel_x, wheel_y;
    float seat_x, seat_y;
    
    left_wheel_x  = x + WHEEL_SIZE;
    right_wheel_x = left_wheel_x + WHEEL_SIZE * 2 + WHEEL_GAP;
    
    wheel_y = y + WHEEL_SIZE + SEAT_GAP;
    
    seat_x = (right_wheel_x - left_wheel_x) / 2.0f + left_wheel_x;
    seat_y = y + SEAT_GAP;
    
    draw_circle(bike_color, left_wheel_x, wheel_y, WHEEL_SIZE);
    draw_circle(bike_color, right_wheel_x, wheel_y, WHEEL_SIZE);
    draw_triangle(bike_color, left_wheel_x, wheel_y, right_wheel_x, wheel_y, seat_x, seat_y);
    draw_line(bike_color, right_wheel_x, wheel_y, right_wheel_x, y);
}

// ======================
// = Main - Entry Point =
// ======================

// Run the bike race...
int main()
{
    open_audio();
    open_graphics_window("Bicycle Race...", 800, 600);
    load_default_colors();
    
    clear_screen();
    
    draw_bike(ColorRed, bike_x_for_accel(random_accel()), 10);
    draw_bike(ColorGreen, bike_x_for_accel(random_accel()), 60);
    draw_bike(ColorBlue, bike_x_for_accel(random_accel()), 110);
    draw_bike(rgbcolor(127, 127, 0), bike_x_for_accel(random_accel()), 160);
    draw_bike(rgbcolor(127, 127, 127), bike_x_for_accel(random_accel()), 210);
    draw_bike(rgbcolor(255, 255, 255), bike_x_for_accel(random_accel()), 260);
    draw_bike(random_color(), bike_x_for_accel(random_accel()), 310);
    
    refresh_screen();
    
    delay(5000);
    
    close_audio();
    
    release_all_resources();
    return 0;
}
