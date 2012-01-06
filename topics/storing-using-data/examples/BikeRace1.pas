  // ==============
  // = Procedures =
  // ==============

  // Draw the bike to the screen in the given color
  procedure DrawBike(bikeColor: Color; x, y: Single);
  var
    leftWheelX, rightWheelX, wheelY: Single;
    seatX, seatY: Single;
  begin
      leftWheelX  := x + WHEEL_SIZE;
      rightWheelX := leftWheelX + WHEEL_SIZE * 2 + WHEEL_GAP;
      
      wheelY := y + WHEEL_SIZE + SEAT_GAP;

      seatX := (rightWheelX - leftWheelX) / 2.0 + leftWheelX;
      seatY := y + SEAT_GAP;

      DrawCircle(bikeColor, leftWheelX, wheelY, WHEEL_SIZE);
      DrawCircle(bikeColor, rightWheelX, wheelY, WHEEL_SIZE);
      DrawTriangle(bikeColor, leftWheelX, wheelY, 
                              rightWheelX, wheelY, 
                              seatX, seatY);
      DrawLine(bikeColor, rightWheelX, wheelY, rightWheelX, y);
  end;

  // ======================
  // = Main - Entry Point =
  // ======================
procedure Main();
begin
  OpenGraphicsWindow('Bike Race', 800, 600);
  
  ClearScreen(ColorWhite);

  DrawBike(ColorRed, BikeXForAccel(RandomAccel()), 10);
  DrawBike(ColorGreen, BikeXForAccel(RandomAccel()), 60);
  DrawBike(ColorBlue, BikeXForAccel(RandomAccel()), 110);
  DrawBike(RGBColor(127, 127, 0), BikeXForAccel(RandomAccel()), 160);
  DrawBike(RGBColor(127, 127, 127), BikeXForAccel(RandomAccel()), 210);
  DrawBike(RGBColor(0, 0, 0), BikeXForAccel(RandomAccel()), 260);
  DrawBike(RandomColor(), BikeXForAccel(RandomAccel()), 310);
  RefreshScreen();

  Delay(5000);
  
  ReleaseAllResources();
end;

begin
  Main();
end.
