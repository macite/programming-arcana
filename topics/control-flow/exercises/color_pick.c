void draw_colored_rects()
{
    do
    {
        process_events();
        clear_screen();
        
        if (key_down(VK_R)) fill_rectangle(ColorRed, 10, 10, 780, 580);
        if (key_down(VK_G)) fill_rectangle(ColorGreen, 20, 20, 760, 560);
        if (key_down(VK_B)) fill_rectangle(ColorBlue, 30, 30, 740, 540);
        
        refresh_screen();
    } while ( ! key_typed(VK_ESCAPE) );
}