// Program: Draw Sun Scene
program GameMain;
uses sgGraphics, sgUtils, sgResources;

procedure DrawSun();
begin
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
end;

procedure Main();
begin
    OpenGraphicsWindow('Draw Sun', 800, 600);
    
    ClearScreen(ColorBlue);
    DrawSun();
    
    RefreshScreen();
    Delay(5000);
    
    ReleaseAllResources();
end;

begin
    Main();
end.
