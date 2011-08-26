void play_game()
{
    int x = 380, y = 280;
    color clr = random_rgbcolor(255); // Opaque color
    
    do
    {
        process_events();    
        if (mouse_clicked(LEFT_BUTTON) && 
            point_in_rect(mouse_x(), mouse_y(), x, y, 40, 40))
        {
            clr = random_rgbcolor(255);
        }
        
        clear_screen(ColorWhite);
        fill_rectangle(clr, x, y, 40, 40);
        refresh_screen();
        
        x = x + (rnd() * 6.0) - 3;
        y = y + (rnd() * 6.0) - 3;
    } while ( ! (key_typed(VK_ESCAPE) || window_close_requested()) );
}
