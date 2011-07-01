program GameMain;
uses sgGraphics, sgUtils, sgResources;

procedure Main();
begin
    OpenGraphicsWindow('Long Code', 800, 600);
    
    //First Background
    //Draw the background
    FillRectangle(ColorBlue, 0, 0, 800, 300);
    FillRectangle(ColorGreen, 0, 300, 800, 300);
    RefreshScreen();
    
    Delay(1000);
    
    //Second Sun
    //Draw the background
    FillRectangle(ColorBlue, 0, 0, 800, 300);
    FillRectangle(ColorGreen, 0, 300, 800, 300);
    RefreshScreen();

    Delay(200);

    //Draw the sun
    FillCircle(ColorYellow, 50.0, 50.0, 20);
    DrawCircle(ColorRed, 50.0, 50.0, 15);
    DrawCircle(ColorRed, 50.0, 50.0, 12);
    DrawCircle(ColorRed, 50.0, 50.0, 9);
    DrawCircle(ColorRed, 50.0, 50.0, 6);
    FillCircle(ColorWhite, 50.0, 50.0, 3);
    DrawLine(ColorYellow, 50, 75, 50, 110);
    DrawLine(ColorYellow, 25, 75, 0, 100);
    DrawLine(ColorYellow, 75, 75, 100, 100);
    DrawLine(ColorYellow, 0, 50, 25, 50);
    DrawLine(ColorYellow, 75, 50, 100, 50);
    DrawLine(ColorYellow, 25, 25, 0, 0);
    DrawLine(ColorYellow, 75, 25, 100, 0);
    DrawLine(ColorYellow, 50, 0, 50, 25);
    RefreshScreen();

    Delay(1000);

    //Third Face
    //Draw the background
    FillRectangle(ColorBlue, 0, 0, 800, 300);
    FillRectangle(ColorGreen, 0, 300, 800, 300);
    RefreshScreen();
    Delay(200);
    
    //Draw the sun
    FillCircle(ColorYellow, 50.0, 50.0, 20);
    DrawCircle(ColorRed, 50.0, 50.0, 15);
    DrawCircle(ColorRed, 50.0, 50.0, 12);
    DrawCircle(ColorRed, 50.0, 50.0, 9);
    DrawCircle(ColorRed, 50.0, 50.0, 6);
    FillCircle(ColorWhite, 50.0, 50.0, 3);
    DrawLine(ColorYellow, 50, 75, 50, 110);
    DrawLine(ColorYellow, 25, 75, 0, 100);
    DrawLine(ColorYellow, 75, 75, 100, 100);
    DrawLine(ColorYellow, 0, 50, 25, 50);
    DrawLine(ColorYellow, 75, 50, 100, 50);
    DrawLine(ColorYellow, 25, 25, 0, 0);
    DrawLine(ColorYellow, 75, 25, 100, 0);
    DrawLine(ColorYellow, 50, 0, 50, 25);
    RefreshScreen();
    Delay(200);

    //Draw Face
    FillCircle(ColorPink, 100, 250, 80);
    FillEllipse(ColorWhite, 40, 240, 40, 30);
    FillEllipse(ColorWhite, 120, 240, 40, 30);
    FillCircle(ColorBlue, 67, 260, 10);
    FillCircle(ColorBlue, 132, 260, 10);        
    FillTriangle(ColorRed, 100, 240, 85, 280, 115, 280);    
    DrawTriangle(ColorBlack, 100, 240, 85, 280, 115, 280);  
    FillTriangle(ColorRed, 100, 290, 100, 310, 70, 300);    
    FillTriangle(ColorRed, 100, 290, 100, 310, 130, 300);   
    DrawTriangle(ColorBlack, 100, 290, 100, 310, 70, 300);  
    DrawTriangle(ColorBlack, 100, 290, 100, 310, 130, 300); 

    RefreshScreen();

    Delay(1000);
    
    //Fourth House
    //Draw the background
    FillRectangle(ColorBlue, 0, 0, 800, 300);
    FillRectangle(ColorGreen, 0, 300, 800, 300);
    RefreshScreen();
    Delay(200);
    
    //Draw the sun
    FillCircle(ColorYellow, 50.0, 50.0, 20);
    DrawCircle(ColorRed, 50.0, 50.0, 15);
    DrawCircle(ColorRed, 50.0, 50.0, 12);
    DrawCircle(ColorRed, 50.0, 50.0, 9);
    DrawCircle(ColorRed, 50.0, 50.0, 6);
    FillCircle(ColorWhite, 50.0, 50.0, 3);
    DrawLine(ColorYellow, 50, 75, 50, 110);
    DrawLine(ColorYellow, 25, 75, 0, 100);
    DrawLine(ColorYellow, 75, 75, 100, 100);
    DrawLine(ColorYellow, 0, 50, 25, 50);
    DrawLine(ColorYellow, 75, 50, 100, 50);
    DrawLine(ColorYellow, 25, 25, 0, 0);
    DrawLine(ColorYellow, 75, 25, 100, 0);
    DrawLine(ColorYellow, 50, 0, 50, 25);
    RefreshScreen();
    Delay(200);

    //Draw Face
    FillCircle(ColorPink, 100, 250, 80);
    FillEllipse(ColorWhite, 40, 240, 40, 30);
    FillEllipse(ColorWhite, 120, 240, 40, 30);
    FillCircle(ColorBlue, 67, 260, 10);
    FillCircle(ColorBlue, 132, 260, 10);        
    FillTriangle(ColorRed, 100, 240, 85, 280, 115, 280);    
    DrawTriangle(ColorBlack, 100, 240, 85, 280, 115, 280);  
    FillTriangle(ColorRed, 100, 290, 100, 310, 70, 300);    
    FillTriangle(ColorRed, 100, 290, 100, 310, 130, 300);   
    DrawTriangle(ColorBlack, 100, 290, 100, 310, 70, 300);  
    DrawTriangle(ColorBlack, 100, 290, 100, 310, 130, 300); 
    RefreshScreen();
    Delay(200);

    //Draw House
    FillTriangle(ColorRed, 300, 160, 200, 260, 400, 260);
    FillRectangle(ColorGrey, 210, 260, 180, 180);
    FillRectangle(ColorBlack, 215, 300, 80, 140);
    FillRectangle(ColorLightGrey, 300, 300, 80, 60);
    FillTriangle(ColorYellow, 345, 300, 380, 300, 380, 355);
    FillTriangle(ColorYellow, 300, 300, 300, 355, 335, 300);
    FillCircle(ColorYellow, 280, 370, 3);
    RefreshScreen();

    Delay(1000);
    
    //Fifth Tree
    //Draw the background
    FillRectangle(ColorBlue, 0, 0, 800, 300);
    FillRectangle(ColorGreen, 0, 300, 800, 300);
    RefreshScreen();
    Delay(200);
    
    //Draw the sun
    FillCircle(ColorYellow, 50.0, 50.0, 20);
    DrawCircle(ColorRed, 50.0, 50.0, 15);
    DrawCircle(ColorRed, 50.0, 50.0, 12);
    DrawCircle(ColorRed, 50.0, 50.0, 9);
    DrawCircle(ColorRed, 50.0, 50.0, 6);
    FillCircle(ColorWhite, 50.0, 50.0, 3);
    DrawLine(ColorYellow, 50, 75, 50, 110);
    DrawLine(ColorYellow, 25, 75, 0, 100);
    DrawLine(ColorYellow, 75, 75, 100, 100);
    DrawLine(ColorYellow, 0, 50, 25, 50);
    DrawLine(ColorYellow, 75, 50, 100, 50);
    DrawLine(ColorYellow, 25, 25, 0, 0);
    DrawLine(ColorYellow, 75, 25, 100, 0);
    DrawLine(ColorYellow, 50, 0, 50, 25);
    RefreshScreen();
    Delay(200);

    //Draw Face
    FillCircle(ColorPink, 100, 250, 80);
    FillEllipse(ColorWhite, 40, 240, 40, 30);
    FillEllipse(ColorWhite, 120, 240, 40, 30);
    FillCircle(ColorBlue, 67, 260, 10);
    FillCircle(ColorBlue, 132, 260, 10);        
    FillTriangle(ColorRed, 100, 240, 85, 280, 115, 280);    
    DrawTriangle(ColorBlack, 100, 240, 85, 280, 115, 280);  
    FillTriangle(ColorRed, 100, 290, 100, 310, 70, 300);    
    FillTriangle(ColorRed, 100, 290, 100, 310, 130, 300);   
    DrawTriangle(ColorBlack, 100, 290, 100, 310, 70, 300);  
    DrawTriangle(ColorBlack, 100, 290, 100, 310, 130, 300); 
    RefreshScreen();
    Delay(200);

    //Draw House
    FillTriangle(ColorRed, 300, 160, 200, 260, 400, 260);
    FillRectangle(ColorGrey, 210, 260, 180, 180);
    FillRectangle(ColorBlack, 215, 300, 80, 140);
    FillRectangle(ColorLightGrey, 300, 300, 80, 60);
    FillTriangle(ColorYellow, 345, 300, 380, 300, 380, 355);
    FillTriangle(ColorYellow, 300, 300, 300, 355, 335, 300);
    FillCircle(ColorYellow, 280, 370, 3);
    RefreshScreen();
    Delay(200);

    //Draw Tree
    FillTriangle(ColorGreen, 500, 75, 440, 150, 560, 150);          
    FillEllipse(ColorTurquoise, 400, 380, 200, 100);
    FillRectangle(ColorBlack, 495, 274, 20, 160);
    FillTriangle(ColorGreen, 500, 120, 400, 275, 600, 275);
    FillCircle(ColorRed, 490, 150.0, 3);
    FillCircle(ColorRed, 510, 170, 4);
    FillCircle(ColorRed, 480, 240, 3);
    FillCircle(ColorRed, 560, 260, 5);
    RefreshScreen();

    Delay(1000);
    
    //Sixth Flats
    //Draw the background
    FillRectangle(ColorBlue, 0, 0, 800, 300);
    FillRectangle(ColorGreen, 0, 300, 800, 300);
    RefreshScreen();
    Delay(200);
    
    //Draw the sun
    FillCircle(ColorYellow, 50.0, 50.0, 20);
    DrawCircle(ColorRed, 50.0, 50.0, 15);
    DrawCircle(ColorRed, 50.0, 50.0, 12);
    DrawCircle(ColorRed, 50.0, 50.0, 9);
    DrawCircle(ColorRed, 50.0, 50.0, 6);
    FillCircle(ColorWhite, 50.0, 50.0, 3);
    DrawLine(ColorYellow, 50, 75, 50, 110);
    DrawLine(ColorYellow, 25, 75, 0, 100);
    DrawLine(ColorYellow, 75, 75, 100, 100);
    DrawLine(ColorYellow, 0, 50, 25, 50);
    DrawLine(ColorYellow, 75, 50, 100, 50);
    DrawLine(ColorYellow, 25, 25, 0, 0);
    DrawLine(ColorYellow, 75, 25, 100, 0);
    DrawLine(ColorYellow, 50, 0, 50, 25);
    RefreshScreen();
    Delay(200);

    //Draw Face
    FillCircle(ColorPink, 100, 250, 80);
    FillEllipse(ColorWhite, 40, 240, 40, 30);
    FillEllipse(ColorWhite, 120, 240, 40, 30);
    FillCircle(ColorBlue, 67, 260, 10);
    FillCircle(ColorBlue, 132, 260, 10);        
    FillTriangle(ColorRed, 100, 240, 85, 280, 115, 280);    
    DrawTriangle(ColorBlack, 100, 240, 85, 280, 115, 280);  
    FillTriangle(ColorRed, 100, 290, 100, 310, 70, 300);    
    FillTriangle(ColorRed, 100, 290, 100, 310, 130, 300);   
    DrawTriangle(ColorBlack, 100, 290, 100, 310, 70, 300);  
    DrawTriangle(ColorBlack, 100, 290, 100, 310, 130, 300); 
    RefreshScreen();
    Delay(200);

    //Draw House
    FillTriangle(ColorRed, 300, 160, 200, 260, 400, 260);
    FillRectangle(ColorGrey, 210, 260, 180, 180);
    FillRectangle(ColorBlack, 215, 300, 80, 140);
    FillRectangle(ColorLightGrey, 300, 300, 80, 60);
    FillTriangle(ColorYellow, 345, 300, 380, 300, 380, 355);
    FillTriangle(ColorYellow, 300, 300, 300, 355, 335, 300);
    FillCircle(ColorYellow, 280, 370, 3);
    RefreshScreen();
    Delay(200);

    //Draw Tree
    FillTriangle(ColorGreen, 500, 75, 440, 150, 560, 150);          
    FillEllipse(ColorTurquoise, 400, 380, 200, 100);
    FillRectangle(ColorBlack, 495, 274, 20, 160);
    FillTriangle(ColorGreen, 500, 120, 400, 275, 600, 275);
    FillCircle(ColorRed, 490, 150.0, 3);
    FillCircle(ColorRed, 510, 170, 4);
    FillCircle(ColorRed, 480, 240, 3);
    FillCircle(ColorRed, 560, 260, 5);
    RefreshScreen();
    Delay(200);

    //Draw the block
    FillRectangle(ColorRed, 620, 50, 160, 450);
    FillRectangle(ColorBlack, 640, 75, 40, 60);
    FillRectangle(ColorLightGrey, 720, 75, 40, 60);
    FillRectangle(ColorGrey, 640, 225, 40, 60);
    FillRectangle(ColorYellow, 720, 225, 40, 60);
    FillRectangle(ColorBlack, 670, 380, 60, 120);
    RefreshScreen();
    
    Delay(2000);
    
    ReleaseAllResources();
end;

begin
    Main();
end.