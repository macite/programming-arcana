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
