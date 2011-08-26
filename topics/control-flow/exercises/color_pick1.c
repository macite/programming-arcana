void draw_colored_rects_v2()
{
    do
    {
        process_events();
        clear_screen();
        
        if (key_down(VK_R)) fill_rectangle(ColorRed, 10, 10, 780, 580);
        else if (key_down(VK_G)) fill_rectangle(ColorGreen, 20, 20, 760, 560);
        else if (key_down(VK_B)) fill_rectangle(ColorBlue, 30, 30, 740, 540);
        else fill_rectangle(ColorGrey, 40, 40, 720, 520);
        
        refresh_screen();
    } while ( ! ( key_typed(VK_ESCAPE) || window_close_requested() ) );
}