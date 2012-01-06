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

