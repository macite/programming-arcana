//
// Program: circle_areas.c
// Displays the Circle Areas for circles with radius 
// from 1.0 to 5.0 with increments of 0.5.
//

program CircleAreas;

const PI = 3.1415;

function CircleArea(radius: Double): Double;
begin
    result := PI * radius * radius;
end;

procedure Main();
begin
    WriteLn('Circle Areas');
    WriteLn('-----------------');
    WriteLn(' Radius: 1.0 = ', CircleArea(1.0):4:2);
    WriteLn(' Radius: 1.5 = ', CircleArea(1.5):4:2);
    WriteLn(' Radius: 2.0 = ', CircleArea(2.0):4:2);
    WriteLn(' Radius: 2.5 = ', CircleArea(2.5):4:2);
    WriteLn(' Radius: 3.0 = ', CircleArea(3.0):4:2);
    WriteLn(' Radius: 3.5 = ', CircleArea(3.5):4:2);
    WriteLn(' Radius: 4.0 = ', CircleArea(4.0):4:2);
    WriteLn(' Radius: 4.5 = ', CircleArea(4.5):4:2);
    WriteLn(' Radius: 5.0 = ', CircleArea(5.0):4:2);
    WriteLn('-----------------');
end;

begin
    Main();
end.
