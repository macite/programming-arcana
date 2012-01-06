program ButtonClick;
uses sgGraphics, sgInput, sgText, sgTypes;

const
  BTN_X = 100;    BTN_Y = 100;
  BTN_W = 200;    BTN_H = 200;

function MouseOver(x, y, width, height: Integer): Boolean;
var
  mx, my: Single;
begin
    mx := MouseX();
    my := MouseY();
    
    result := (mx >= x) and (mx <= x + width) and 
              (my >= y) and (my <= y + height);
end;

function ButtonClicked(x, y, width, height: Integer): Boolean;
begin
    result := MouseClicked(LeftButton) and MouseOver(x, y, width, height);
end;

// Draw a rectangle moving across the screen
procedure Main();
begin
    OpenGraphicsWindow('Button Click', 800, 600);
    
    repeat
        ProcessEvents();
        
        // Clear the screen, then draw the "button"
        ClearScreen(ColorWhite);
        
        if MouseDown(LeftButton) and MouseOver(BTN_X, BTN_Y, BTN_W, BTN_H) then
            FillRectangle(ColorBlue, BTN_X, BTN_Y, BTN_W, BTN_H)
        else
            DrawRectangle(ColorBlue, BTN_X, BTN_Y, BTN_W, BTN_H);
        
        if ButtonClicked(BTN_X, BTN_Y, BTN_W, BTN_H) then
        begin
            DrawText('CLICKED', ColorBlue, 0, 20);
        end;
        
        DrawFramerate(0,0);
        
        // Refresh the screen, keep it at 60fps
        RefreshScreen(60);
    until WindowCloseRequested();
end;

begin
  Main();
end.