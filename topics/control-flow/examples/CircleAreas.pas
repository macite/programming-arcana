//
// Program: circle_areas.c
// Displays the Circle Areas for circles with radius 
// from 1.0 to 5.0 with increments of 0.1.
//

program CircleAreas;

const PI = 3.1415;
const START_RADIUS = 1.0;
const END_RADIUS   = 5.0;
const RADIUS_INC   = 0.1;

function CircleArea(radius: Double): Double;
begin
    result := PI * radius * radius;
end;

procedure Main();
var
    radius: Double;
begin
    WriteLn('Circle Areas');
    WriteLn('-----------------');
    radius := START_RADIUS;
    
    while radius <= END_RADIUS do
    begin
        WriteLn(' Radius: ', radius:4:2, ' = ', CircleArea(radius):4:2);
        radius := radius + RADIUS_INC;
    end;
    WriteLn('-----------------');
end;

begin
    Main();
end.
