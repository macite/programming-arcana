/* Program: bike-race.c, splashkit */

#include "splashkit.h"

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
