program MovingRect;
uses sgGraphics, sgUtils, sgInput, sgText;

const
  RECT_WIDTH = 100;
  RECT_HEIGHT = 100;
  MOVE_X = 5;

// Update the x position of the rectangle, by the specified amount
procedure UpdateRectPosition(var x, dx: Integer);
begin
    // Move x (passed in by reference)
    x += dx;
    
    // Check if it went of the screen
    if x < 0 then
    begin
        // off the left of the screen
        dx := -dx;   // change movement direction
        x := 0;      // put it back on the screen
    end
    else if (x + RECT_WIDTH) > ScreenWidth() then
    begin
        // off the screen to the right
        dx := -dx;   // change movement direction
        x := (ScreenWidth() - RECT_WIDTH); // put it back on the screen
    end;
end;

// Draw a rectangle moving across the screen
procedure Main();
var
    rectX: Integer = 0;
    rectY: Integer = 250;
    rectXMove: Integer = MOVE_X;
begin
    OpenGraphicsWindow('Moving Rectangle', 800, 600);
    
    repeat
        ProcessEvents();
        
        // Update the location of the rectangle
        UpdateRectPosition(rectX, rectXMove);
        
        // Clear the screen, then draw the rectangle
        ClearScreen(ColorWhite);
        FillRectangle(ColorRed, rectX, rectY, RECT_WIDTH, RECT_HEIGHT);
        DrawFramerate(0,0);
        
        RefreshScreen(60); // Refresh the screen, keep it at 60fps
    until WindowCloseRequested();
end;

begin
  Main();
end.
