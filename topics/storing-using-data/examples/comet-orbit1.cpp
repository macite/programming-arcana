
void draw_comet(float angle)
{
    float r, comet_x, comet_y, screen_x, screen_y;
    
    r = hale_bopp_r(angle);
    comet_x = r * cosine(angle);
    comet_y = r * sine(angle);
    
    screen_y = comet_x_to_screen_y(comet_x);
    screen_x = comet_y_to_screen_x(comet_y);
    
    printf("Comet position (%f deg): %.1f,%.1f = %.1f,%.1f\n", 
            angle, comet_x, comet_y, screen_x, screen_y);
    
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
    int ms = MIN_WAIT + (int)(MAX_POPORTION_WAIT * radius / MAX_DISTANCE);
    
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