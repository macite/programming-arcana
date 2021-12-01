#include "splashkit.h"

int main()
{
    string line;
    int width;
    int height;

    write("How wide do you want the window? Width: ");
    line = read_line();
    width = convert_to_integer(line);

    write("How high do you want the window? Height: ");
    line = read_line();
    height = convert_to_integer(line);


    open_window("Andrew's House Drawing", width, height);

    double hill_y = screen_height() * 2 / 3;
    double hill_height = screen_height() * 2 / 3;

    double house_wall_width = screen_width() / 4;
    double house_wall_height = screen_height() * 3 / 8;
    double house_middle_x = screen_width() / 2;
    double house_wall_top = screen_height() / 2;

    double house_x = house_middle_x - house_wall_width / 2;

    double house_roof_top = screen_height() / 4;
    double roof_gap = house_wall_width / 4;
    double roof_left = house_x - roof_gap;
    double roof_right = house_x + house_wall_width + roof_gap;

    clear_screen(COLOR_WHITE);
    fill_ellipse(COLOR_BRIGHT_GREEN, 0, hill_y, screen_width(), hill_height);
    fill_rectangle(COLOR_GRAY, house_x, house_wall_top, house_wall_width, house_wall_height);
    fill_triangle(COLOR_RED, roof_left, house_wall_top, house_middle_x, house_roof_top, roof_right, house_wall_top);
    refresh_screen();

    delay(5000);

    return 0;
}
