const LARGE_RADIUS = 6;
const SMALL_RADIUS = 3;

procedure DrawCursor();
var
    dotX, dotY: Single;
begin
    HideMouse();
    
    while not WindowCloseRequested() do
    begin
        ProcessEvents();
        
        dotX = MouseX();
        dotY = MouseY();
        
        ClearScreen(ColorWhite);
        DrawFramerate(0,0);
        DrawCircle(ColorBlack, dotX, dotY, LARGE_RADIUS);
        
        if ( MouseDown(LEFT_BUTTON) )
            fill_circle(ColorBlack, dotX, dotY, SMALL_RADIUS)
        else
            DrawCircle(ColorBlack, dotX, dotY, SMALL_RADIUS);
        
        RefreshScreen();
    end;
    
    ShowMouse();
end;