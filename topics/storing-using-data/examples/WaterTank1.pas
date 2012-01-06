procedure Main();
begin
  OpenGraphicsWindow('Water Tanks', 800, 600);
  
  ClearScreen(ColorWhite);
  DrawWaterTank(10, 50, 100, 200, 0.75);
  DrawWaterTank(150, 50, 100, 300, 0.0);
  DrawWaterTank(300, 50, 70, 100, 0.25);
  DrawWaterTank(450, 50, Round(rnd() * MAX_HEIGHT), 
                Round(rnd() * MAX_WIDTH), 0.25);
  
  RefreshScreen();

  Delay(5000);
  
  ReleaseAllResources();
end;

begin
  Main();
end.
