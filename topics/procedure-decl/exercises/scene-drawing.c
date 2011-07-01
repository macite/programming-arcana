#include "SwinGame.h"

int main(int argc, char* argv[])
{
    open_graphics_window("Long Code", 800, 600);
    load_default_colors();
    
    //First Background
    //Draw the background
    fill_rectangle(ColorBlue, 0, 0, 800, 300);
    fill_rectangle(ColorGreen, 0, 300, 800, 300);
    refresh_screen();
    
    delay(1000);
    
    //Second Sun
    //Draw the background
    fill_rectangle(ColorBlue, 0, 0, 800, 300);
    fill_rectangle(ColorGreen, 0, 300, 800, 300);
    refresh_screen();

    delay(200);

    //Draw the sun
    fill_circle(ColorYellow, 50.0, 50.0, 20);
    draw_circle(ColorRed, 50.0, 50.0, 15);
    draw_circle(ColorRed, 50.0, 50.0, 12);
    draw_circle(ColorRed, 50.0, 50.0, 9);
    draw_circle(ColorRed, 50.0, 50.0, 6);
    fill_circle(ColorWhite, 50.0, 50.0, 3);
    draw_line(ColorYellow, 50, 75, 50, 110);
    draw_line(ColorYellow, 25, 75, 0, 100);
    draw_line(ColorYellow, 75, 75, 100, 100);
    draw_line(ColorYellow, 0, 50, 25, 50);
    draw_line(ColorYellow, 75, 50, 100, 50);
    draw_line(ColorYellow, 25, 25, 0, 0);
    draw_line(ColorYellow, 75, 25, 100, 0);
    draw_line(ColorYellow, 50, 0, 50, 25);
    refresh_screen();

    delay(1000);

    //Third Face
    //Draw the background
    fill_rectangle(ColorBlue, 0, 0, 800, 300);
    fill_rectangle(ColorGreen, 0, 300, 800, 300);
    refresh_screen();
    delay(200);
    
    //Draw the sun
    fill_circle(ColorYellow, 50.0, 50.0, 20);
    draw_circle(ColorRed, 50.0, 50.0, 15);
    draw_circle(ColorRed, 50.0, 50.0, 12);
    draw_circle(ColorRed, 50.0, 50.0, 9);
    draw_circle(ColorRed, 50.0, 50.0, 6);
    fill_circle(ColorWhite, 50.0, 50.0, 3);
    draw_line(ColorYellow, 50, 75, 50, 110);
    draw_line(ColorYellow, 25, 75, 0, 100);
    draw_line(ColorYellow, 75, 75, 100, 100);
    draw_line(ColorYellow, 0, 50, 25, 50);
    draw_line(ColorYellow, 75, 50, 100, 50);
    draw_line(ColorYellow, 25, 25, 0, 0);
    draw_line(ColorYellow, 75, 25, 100, 0);
    draw_line(ColorYellow, 50, 0, 50, 25);
    refresh_screen();
    delay(200);

    //Draw Face
    fill_circle(ColorPink, 100, 250, 80);
    fill_ellipse(ColorWhite, 40, 240, 40, 30);
    fill_ellipse(ColorWhite, 120, 240, 40, 30);
    fill_circle(ColorBlue, 67, 260, 10);
    fill_circle(ColorBlue, 132, 260, 10);       
    fill_triangle(ColorRed, 100, 240, 85, 280, 115, 280);   
    draw_triangle(ColorBlack, 100, 240, 85, 280, 115, 280); 
    fill_triangle(ColorRed, 100, 290, 100, 310, 70, 300);   
    fill_triangle(ColorRed, 100, 290, 100, 310, 130, 300);  
    draw_triangle(ColorBlack, 100, 290, 100, 310, 70, 300); 
    draw_triangle(ColorBlack, 100, 290, 100, 310, 130, 300);    

    refresh_screen();

    delay(1000);
    
    //Fourth House
    //Draw the background
    fill_rectangle(ColorBlue, 0, 0, 800, 300);
    fill_rectangle(ColorGreen, 0, 300, 800, 300);
    refresh_screen();
    delay(200);
    
    //Draw the sun
    fill_circle(ColorYellow, 50.0, 50.0, 20);
    draw_circle(ColorRed, 50.0, 50.0, 15);
    draw_circle(ColorRed, 50.0, 50.0, 12);
    draw_circle(ColorRed, 50.0, 50.0, 9);
    draw_circle(ColorRed, 50.0, 50.0, 6);
    fill_circle(ColorWhite, 50.0, 50.0, 3);
    draw_line(ColorYellow, 50, 75, 50, 110);
    draw_line(ColorYellow, 25, 75, 0, 100);
    draw_line(ColorYellow, 75, 75, 100, 100);
    draw_line(ColorYellow, 0, 50, 25, 50);
    draw_line(ColorYellow, 75, 50, 100, 50);
    draw_line(ColorYellow, 25, 25, 0, 0);
    draw_line(ColorYellow, 75, 25, 100, 0);
    draw_line(ColorYellow, 50, 0, 50, 25);
    refresh_screen();
    delay(200);

    //Draw Face
    fill_circle(ColorPink, 100, 250, 80);
    fill_ellipse(ColorWhite, 40, 240, 40, 30);
    fill_ellipse(ColorWhite, 120, 240, 40, 30);
    fill_circle(ColorBlue, 67, 260, 10);
    fill_circle(ColorBlue, 132, 260, 10);       
    fill_triangle(ColorRed, 100, 240, 85, 280, 115, 280);   
    draw_triangle(ColorBlack, 100, 240, 85, 280, 115, 280); 
    fill_triangle(ColorRed, 100, 290, 100, 310, 70, 300);   
    fill_triangle(ColorRed, 100, 290, 100, 310, 130, 300);  
    draw_triangle(ColorBlack, 100, 290, 100, 310, 70, 300); 
    draw_triangle(ColorBlack, 100, 290, 100, 310, 130, 300);    
    refresh_screen();
    delay(200);

    //Draw House
    fill_triangle(ColorRed, 300, 160, 200, 260, 400, 260);
    fill_rectangle(ColorGrey, 210, 260, 180, 180);
    fill_rectangle(ColorBlack, 215, 300, 80, 140);
    fill_rectangle(ColorLightGrey, 300, 300, 80, 60);
    fill_triangle(ColorYellow, 345, 300, 380, 300, 380, 355);
    fill_triangle(ColorYellow, 300, 300, 300, 355, 335, 300);
    fill_circle(ColorYellow, 280, 370, 3);
    refresh_screen();

    delay(1000);
    
    //Fifth Tree
    //Draw the background
    fill_rectangle(ColorBlue, 0, 0, 800, 300);
    fill_rectangle(ColorGreen, 0, 300, 800, 300);
    refresh_screen();
    delay(200);
    
    //Draw the sun
    fill_circle(ColorYellow, 50.0, 50.0, 20);
    draw_circle(ColorRed, 50.0, 50.0, 15);
    draw_circle(ColorRed, 50.0, 50.0, 12);
    draw_circle(ColorRed, 50.0, 50.0, 9);
    draw_circle(ColorRed, 50.0, 50.0, 6);
    fill_circle(ColorWhite, 50.0, 50.0, 3);
    draw_line(ColorYellow, 50, 75, 50, 110);
    draw_line(ColorYellow, 25, 75, 0, 100);
    draw_line(ColorYellow, 75, 75, 100, 100);
    draw_line(ColorYellow, 0, 50, 25, 50);
    draw_line(ColorYellow, 75, 50, 100, 50);
    draw_line(ColorYellow, 25, 25, 0, 0);
    draw_line(ColorYellow, 75, 25, 100, 0);
    draw_line(ColorYellow, 50, 0, 50, 25);
    refresh_screen();
    delay(200);

    //Draw Face
    fill_circle(ColorPink, 100, 250, 80);
    fill_ellipse(ColorWhite, 40, 240, 40, 30);
    fill_ellipse(ColorWhite, 120, 240, 40, 30);
    fill_circle(ColorBlue, 67, 260, 10);
    fill_circle(ColorBlue, 132, 260, 10);       
    fill_triangle(ColorRed, 100, 240, 85, 280, 115, 280);   
    draw_triangle(ColorBlack, 100, 240, 85, 280, 115, 280); 
    fill_triangle(ColorRed, 100, 290, 100, 310, 70, 300);   
    fill_triangle(ColorRed, 100, 290, 100, 310, 130, 300);  
    draw_triangle(ColorBlack, 100, 290, 100, 310, 70, 300); 
    draw_triangle(ColorBlack, 100, 290, 100, 310, 130, 300);    
    refresh_screen();
    delay(200);

    //Draw House
    fill_triangle(ColorRed, 300, 160, 200, 260, 400, 260);
    fill_rectangle(ColorGrey, 210, 260, 180, 180);
    fill_rectangle(ColorBlack, 215, 300, 80, 140);
    fill_rectangle(ColorLightGrey, 300, 300, 80, 60);
    fill_triangle(ColorYellow, 345, 300, 380, 300, 380, 355);
    fill_triangle(ColorYellow, 300, 300, 300, 355, 335, 300);
    fill_circle(ColorYellow, 280, 370, 3);
    refresh_screen();
    delay(200);

    //Draw Tree
    fill_triangle(ColorGreen, 500, 75, 440, 150, 560, 150);         
    fill_ellipse(ColorTurquoise, 400, 380, 200, 100);
    fill_rectangle(ColorBlack, 495, 274, 20, 160);
    fill_triangle(ColorGreen, 500, 120, 400, 275, 600, 275);
    fill_circle(ColorRed, 490, 150.0, 3);
    fill_circle(ColorRed, 510, 170, 4);
    fill_circle(ColorRed, 480, 240, 3);
    fill_circle(ColorRed, 560, 260, 5);
    refresh_screen();

    delay(1000);
    
    //Sixth Flats
    //Draw the background
    fill_rectangle(ColorBlue, 0, 0, 800, 300);
    fill_rectangle(ColorGreen, 0, 300, 800, 300);
    refresh_screen();
    delay(200);
    
    //Draw the sun
    fill_circle(ColorYellow, 50.0, 50.0, 20);
    draw_circle(ColorRed, 50.0, 50.0, 15);
    draw_circle(ColorRed, 50.0, 50.0, 12);
    draw_circle(ColorRed, 50.0, 50.0, 9);
    draw_circle(ColorRed, 50.0, 50.0, 6);
    fill_circle(ColorWhite, 50.0, 50.0, 3);
    draw_line(ColorYellow, 50, 75, 50, 110);
    draw_line(ColorYellow, 25, 75, 0, 100);
    draw_line(ColorYellow, 75, 75, 100, 100);
    draw_line(ColorYellow, 0, 50, 25, 50);
    draw_line(ColorYellow, 75, 50, 100, 50);
    draw_line(ColorYellow, 25, 25, 0, 0);
    draw_line(ColorYellow, 75, 25, 100, 0);
    draw_line(ColorYellow, 50, 0, 50, 25);
    refresh_screen();
    delay(200);

    //Draw Face
    fill_circle(ColorPink, 100, 250, 80);
    fill_ellipse(ColorWhite, 40, 240, 40, 30);
    fill_ellipse(ColorWhite, 120, 240, 40, 30);
    fill_circle(ColorBlue, 67, 260, 10);
    fill_circle(ColorBlue, 132, 260, 10);       
    fill_triangle(ColorRed, 100, 240, 85, 280, 115, 280);   
    draw_triangle(ColorBlack, 100, 240, 85, 280, 115, 280); 
    fill_triangle(ColorRed, 100, 290, 100, 310, 70, 300);   
    fill_triangle(ColorRed, 100, 290, 100, 310, 130, 300);  
    draw_triangle(ColorBlack, 100, 290, 100, 310, 70, 300); 
    draw_triangle(ColorBlack, 100, 290, 100, 310, 130, 300);    
    refresh_screen();
    delay(200);

    //Draw House
    fill_triangle(ColorRed, 300, 160, 200, 260, 400, 260);
    fill_rectangle(ColorGrey, 210, 260, 180, 180);
    fill_rectangle(ColorBlack, 215, 300, 80, 140);
    fill_rectangle(ColorLightGrey, 300, 300, 80, 60);
    fill_triangle(ColorYellow, 345, 300, 380, 300, 380, 355);
    fill_triangle(ColorYellow, 300, 300, 300, 355, 335, 300);
    fill_circle(ColorYellow, 280, 370, 3);
    refresh_screen();
    delay(200);

    //Draw Tree
    fill_triangle(ColorGreen, 500, 75, 440, 150, 560, 150);         
    fill_ellipse(ColorTurquoise, 400, 380, 200, 100);
    fill_rectangle(ColorBlack, 495, 274, 20, 160);
    fill_triangle(ColorGreen, 500, 120, 400, 275, 600, 275);
    fill_circle(ColorRed, 490, 150.0, 3);
    fill_circle(ColorRed, 510, 170, 4);
    fill_circle(ColorRed, 480, 240, 3);
    fill_circle(ColorRed, 560, 260, 5);
    refresh_screen();
    delay(200);

    //Draw the block
    fill_rectangle(ColorRed, 620, 50, 160, 450);
    fill_rectangle(ColorBlack, 640, 75, 40, 60);
    fill_rectangle(ColorLightGrey, 720, 75, 40, 60);
    fill_rectangle(ColorGrey, 640, 225, 40, 60);
    fill_rectangle(ColorYellow, 720, 225, 40, 60);
    fill_rectangle(ColorBlack, 670, 380, 60, 120);
    refresh_screen();
    
    delay(2000);

    release_all_resources();
    return 0;
}
