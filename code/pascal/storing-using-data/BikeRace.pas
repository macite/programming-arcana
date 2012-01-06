program GameMain;
uses
  sgTypes, sgInput, sgAudio, sgGraphics, sgResources, sgUtils, sgText;

  // ====================
  // = Define constants =
  // ====================
  const
    WHEEL_SIZE = 10;
    WHEEL_GAP = 10;
    SEAT_GAP = 5;
    MAX_ACCELERATION = 10;
    RACE_DURATION = 30;
    X_SCALE_FACTOR = 0.15;

  // =============
  // = Functions =
  // =============

  // Calculate the distance travelled given acceleration and time
  // distance = ut + ((at^2) / 2)
  function DistanceTravelled(initialSpeed, acceleration, time: Single): Single;
  begin
      result := (initialSpeed * time) + ((acceleration * time * time) / 2);
  end;
  
  // Calculate the x position of a bike accelerating at the given
  // acceleration for the duration of the race 
  function BikeXForAccel(acceleration: Single): Single;
  var
    distance: Single;
  begin
      distance := DistanceTravelled(0, acceleration, RACE_DURATION);
      result := distance * X_SCALE_FACTOR;
  end;

  // Come up with a random acceleration value for a bike between 0 and 
  // MAX_ACCELERATION
  function RandomAccel() : Single;
  begin
      result := rnd() * MAX_ACCELERATION;
  end;

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
      DrawTriangle(bikeColor, leftWheelX, wheelY, rightWheelX, wheelY, seatX, seatY);
      DrawLine(bikeColor, rightWheelX, wheelY, rightWheelX, y);
  end;

  // ======================
  // = Main - Entry Point =
  // ======================
procedure Main();
begin
  OpenGraphicsWindow('Bike Race', 800, 600);
  
  ClearScreen();

  DrawBike(ColorRed, BikeXForAccel(RandomAccel()), 10);
  DrawBike(ColorGreen, BikeXForAccel(RandomAccel()), 60);
  DrawBike(ColorBlue, BikeXForAccel(RandomAccel()), 110);
  DrawBike(RGBColor(127, 127, 0), BikeXForAccel(RandomAccel()), 160);
  DrawBike(RGBColor(127, 127, 127), BikeXForAccel(RandomAccel()), 210);
  DrawBike(RGBColor(255, 255, 255), BikeXForAccel(RandomAccel()), 260);
  DrawBike(RandomColor(), BikeXForAccel(RandomAccel()), 310);
  RefreshScreen();

  Delay(5000);
  
  ReleaseAllResources();
end;

begin
  Main();
end.
