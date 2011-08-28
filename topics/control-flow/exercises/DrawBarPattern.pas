procedure DrawBarPattern(numBars: Integer);
var
    x, i, barWidth: Integer;
begin
    i := 0;
    barWidth := ScreenWidth() / numBars;
    
    while i < numBars do
    begin
        x := i * barWidth;
        if (i mod 2) = 0 then
            FillRectangle(ColorWhite, x, 0, barWidth, ScreenHeight())
        else
            FillRectangle(ColorBlack, x, 0, barWidth, ScreenHeight());
        
        i += 1;
    end;
    
    RefreshScreen();
end