void draw_grid_pattern(int num_rows, int num_cols)
{
    int x, y, i, j, bar_width, bar_height;
    
    i = 0;
    bar_width = screen_width() / num_cols;
    bar_height = screen_height() / num_rows;
    
    while ( i < num_rows )
    {
        y = i * bar_height;
        j = 0;
        
        while ( j < num_cols )
        {
            x = j * bar_width;
            if ( ((i + j) % 2) == 0 )
                fill_rectangle(ColorWhite, x, y, bar_width, bar_height);
            else
                fill_rectangle(ColorBlack, x, y, bar_width, bar_height);
            
            j++;
        }
        i++;
    }
    refresh_screen();
}