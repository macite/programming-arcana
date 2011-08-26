const X_SPEED = 5;

procedure ????(width, height: Integer);
var
    y: Single;
    x: Single = 0;
begin
  y := (ScreenHeight() - height) / 2;
  
  while (x + width) < ScreenWidth() do
  begin
    ClearScreen();
    
    FillRectangle(ColorRed, x, y, width, height);
    x := x + X_SPEED;
    
    RefreshScreen();
  end; 
end;