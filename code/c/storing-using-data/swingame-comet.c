#include <stdio.h>
#include <stdbool.h>
#include "SwinGame.h"

// Constant values
#define ECCENTRICITY 0.995f
#define DISTANCE_POLE_DIRECTRIX 1.828f
#define SUN_X 10
#define SUN_Y 300
#define SUN_RADIUS 2
#define COMET_RADIUS 1
#define MAX_DISTANCE 380.0f

#define MIN_WAIT 100
#define MAX_POPORTION_WAIT 900

// Calculated constants
const float SCREEN_SCALE_FACTOR = 800.0f / MAX_DISTANCE;

// r = ed / (1 + e sin(angle) )
float hale_bopp_r(float angle)
{
    return ECCENTRICITY * DISTANCE_POLE_DIRECTRIX / (1 + ECCENTRICITY * sine(angle));
}

float comet_y_to_screen_x(float y)
{
    float result;
    
    // Scale to screen coordinates... reverse sign (- to +)
    result = y * -SCREEN_SCALE_FACTOR;
    
    // Translate so 0,0 is at SUN_X,SUNY
    result = result + SUN_X;
    
    return result;
}

float comet_x_to_screen_y(float x)
{
    float result;
    
    // Scale to screen coordinates
    result = x * SCREEN_SCALE_FACTOR;
    
    // Translate so 0,0 is at SUN_X,SUNY
    result = result + SUN_Y;
    
    return result;
}

void draw_comet(float angle)
{
    float r, comet_x, comet_y, screen_x, screen_y;
    
    r = hale_bopp_r(angle);
    comet_x = r * cosine(angle);
    comet_y = r * sine(angle);
    
    screen_y = comet_x_to_screen_y(comet_x);
    screen_x = comet_y_to_screen_x(comet_y);
    
    printf("Comet position (%f deg): %.1f,%.1f = %.1f,%.1f\n", angle, comet_x, comet_y, screen_x, screen_y);
    
    fill_circle(ColorWhite, screen_x, screen_y, COMET_RADIUS);
}

void draw_sun()
{
    fill_circle(ColorYellow, SUN_X, SUN_Y, SUN_RADIUS);
}

void delay_for_angle(float angle)
{
    // Delay is inversly proportional to radius
    float radius = hale_bopp_r(angle);
    int ms = MIN_WAIT + (int)(MAX_POPORTION_WAIT * (radius * radius) / (MAX_DISTANCE * MAX_DISTANCE));
    
    delay(ms);
}

void draw_system(float angle)
{
    clear_screen();
    draw_sun();
    draw_comet(angle);
    refresh_screen();
    delay_for_angle(angle);
}

int main()
{
    open_audio();
    open_graphics_window("Hale-Bopp's Orbit", 800, 600);
    load_default_colors();
    
    draw_system(0);
    draw_system(45);
    draw_system(90);
    draw_system(180);
    draw_system(200);
    draw_system(220);
    draw_system(240);
    draw_system(260);
    draw_system(265);
    draw_system(266);
    draw_system(267);
    draw_system(268);
    draw_system(269);
    draw_system(270);
    draw_system(271);
    draw_system(272);
    draw_system(273);
    draw_system(274);
    draw_system(275);
    draw_system(280);
    draw_system(300);
    draw_system(320);
    draw_system(340);
    draw_system(360);

    close_audio();
    
    release_all_resources();
    return 0;
}
