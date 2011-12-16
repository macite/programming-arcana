program TestFnCalls;

function Square(x: Integer): Integer;
begin
    result := x * x;
end; 

function sum(a, b: Integer): Integer;
begin
    result := a + b;
end;

procedure Main();
var
    answer: Integer;
begin
    answer := sum(square(5), square(4));
    WriteLn('5 squared + 4 squared is ', answer);
    
    WriteLn('(1 + 2) + (3 + 4) = ', Sum(Sum(1, 2), Sum(3, 4)));
    WriteLn('2 squared, squared = ', Square(Square(2)));
end;

begin
    Main();
end.