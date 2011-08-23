// Coordinates the drawing of a number of water tanks
int main(int argc, char* argv[])
{
    open_audio();
    open_graphics_window("Water Tanks", 800, 600);
    load_default_colors();
    
    clear_screen(ColorWhite);
    draw_water_tank(10, 50, 100, 200, 0.75);
    draw_water_tank(150, 50, 100, 300, 0.0);
    draw_water_tank(300, 50, 70, 100, 0.25);
    draw_water_tank(450, 50, rnd() * MAX_HEIGHT, rnd() * MAX_WIDTH, 0.25);
    refresh_screen();
    
    delay(5000);

    release_all_resources();
    close_audio();
    return 0;
}
