//
// Program: ShapeDrawing.pas
// Draws a number of shapes to the screen using SwinGame.
//
program GameMain;
uses
  sgTypes, sgInput, sgAudio, sgGraphics, sgResources, sgUtils, sgText;

begin
    OpenGraphicsWindow('Shape Drawing', 800, 600);

    ClearScreen();

    FillRectangle(ColorWhite, 10, 10, 780, 580);

    RefreshScreen();
    Delay(500);

    FillCircle(ColorRed, 50, 50, 25);
    FillCircle(ColorGreen, 80, 50, 25);
    FillCircle(ColorBlue, 110, 50, 25);

    RefreshScreen();
    Delay(500);

    FillTriangle(ColorYellow, 100, 100, 150, 175, 210, 115);
    RefreshScreen();
    Delay(2000);

    ReleaseAllResources();
end.
