procedure DrawGridPattern(numRows, numCols: Integer);
var
    x, y, i, j, barWidth, barHeight: Integer;
begin
    i = 0;
    barWidth = screen_width() / numCols;
    barHeight = screen_height() / numRows;
    
    while i < numRows do
    begin
        y := i * barHeight;
        j := 0;
        
        while j < numCols do
        begin
            x := j * barWidth;
            if  ((i + j) mod 2) = 0 then
                fill_rectangle(ColorWhite, x, y, barWidth, barHeight)
            else
                fill_rectangle(ColorBlack, x, y, barWidth, barHeight);
            
            j += 1;
        end;
        i += 1;
    end;
    RefreshScreen();
end;