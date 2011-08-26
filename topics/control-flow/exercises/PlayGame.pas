procedure PlayGame();
var
    x: Single = 380.0;
    y: Single = 280.0;
    clr: Color;
begin
    clr := RandomRGBColor(255); // Opaque color
    
    repeat
        process_events();    
        if MouseClicked(LEFT_BUTTON) and
           PointInRect(MouseX(), MouseY(), x, y, 40, 40) then
        begin
            clr = RandomRGBColor(255);
        end;
        
        ClearScreen(ColorWhite);
        FillRectangle(clr, x, y, 40, 40);
        RefreshScreen();
        
        x = x + (rnd() * 6.0) - 3;
        y = y + (rnd() * 6.0) - 3;
    until KeyTyped(vk_ESCAPE) or WindowCloseRequested();
end;
