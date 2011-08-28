void draw_bar_pattern(int num_bars)
{
    int x, i, bar_width;
    
    i = 0;
    bar_width = screen_width() / num_bars;
    
    while( i < num_bars )
    {
        x = i * bar_width;
        if ( (i % 2) == 0 )
            fill_rectangle(ColorWhite, x, 0, bar_width, screen_height());
        else
            fill_rectangle(ColorBlack, x, 0, bar_width, screen_height());
        
        i++;
    }
    refresh_screen();
}