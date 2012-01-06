program TestAlias;

type 
    // Basic alias - number is an alias for int
    Number = Integer;
    
    // Array alias - FiveNumbers is an alias for an array of five integers
    FiveNumbers = array [0..4] of Integer;
    
procedure Test(idx: Number; data: FiveNumbers);
begin
    WriteLn(' -> ', data[idx]);
end;

procedure Main();
var
    var1: Number = 3;
    arr1: FiveNumbers = (1, 2, 3, 4, 5);
    grid: array [0..1] of FiveNumbers = 
            ((1, 2, 3, 4, 5), 
             (6, 7, 8, 9, 10) );
begin
    Test(var1, arr1);
    Test(3, grid[0]);
    Test(var1, grid[1]);
end;

begin
    Main();
end.
