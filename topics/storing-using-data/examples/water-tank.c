#include "SwinGame.h"

#define MAX_HEIGHT 400
#define MAX_WIDTH 200

// Draws a water tank, at a given x, y location in a set width and height
// and at a certain percent full
void draw_water_tank(float x, float y, int width, int height, float pct_full)
{
    float ellipse_height;       // the height of the ellipses for top/botton
    float body_height, body_y;  // the height of the core of the cylinder
    float bottom_ellipse_y, top_ellipse_y; // the y position of the ellipses
    float water_height, water_y; // the top (y) of the water, and its height
    
    ellipse_height = height * 0.1;          // 10% of its height
    body_height = height - ellipse_height;
    body_y = y + ellipse_height / 2;
    bottom_ellipse_y = (y + height) - ellipse_height;
    
    water_height = pct_full * body_height;
    water_y = body_y + (body_height - water_height);
    top_ellipse_y = water_y - ellipse_height / 2;
    
    // Water...
    // Bottom ellipse
    fill_ellipse(ColorBlue, x, bottom_ellipse_y, width, ellipse_height);
    draw_ellipse(ColorBlack, x, bottom_ellipse_y, width, ellipse_height);
    // Body - center of cylinder
    fill_rectangle(ColorBlue, x, water_y, width, water_height);
    //Top ellipse
    fill_ellipse(ColorBlue, x, top_ellipse_y, width, ellipse_height);
    draw_ellipse(ColorBlack, x, top_ellipse_y, width, ellipse_height);
    
    // Frame
    draw_ellipse(ColorBlack, x, y, width, ellipse_height);
    draw_line(ColorBlack, x, y + ellipse_height / 2, x, 
            bottom_ellipse_y + ellipse_height / 2);
    draw_line(ColorBlack, x + width, y + ellipse_height / 2, 
            x + width, bottom_ellipse_y + ellipse_height / 2);
}