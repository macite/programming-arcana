#define X_SPEED 5

void ????(int width, int height)
{
    float x = 0, y;
    
    y = (screen_height() - height) / 2;
    
    while ( (x + width) < screen_width() )
    {
        clear_screen();
        
        fill_rectangle(ColorRed, x, y, width, height);
        x = x + X_SPEED;
        
        refresh_screen();
    }
}
