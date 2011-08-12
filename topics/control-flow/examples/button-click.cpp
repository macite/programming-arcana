#include <stdio.h>
#include <stdbool.h>
#include "SwinGame.h"

#define BTN_X 100
#define BTN_Y 100
#define BTN_W 200
#define BTN_H 200

bool mouse_over(int x, int y, int width, int height)
{
    float mx, my;
    
    mx = mouse_x();
    my = mouse_y();
    
    return mx >= x && mx <= x + width && my >= y && my <= y + height;
    
    // Which is a short cut for...
    // if (mx >= x && mx <= x + width && my >= y && my <= y + height)
    //     return true;
    // else
    //     return false;
}

bool button_clicked(int x, int y, int width, int height)
{
    return mouse_clicked(LEFT_BUTTON) && mouse_over(x, y, width, height);
    
    // Which is a short cut for...
    // if (mouse_clicked(LEFT_BUTTON) && mouse_over(x, y, width, height))
    // {
    //     return true;
    // }
    // else
    // {
    //     return false;
    // }
}
