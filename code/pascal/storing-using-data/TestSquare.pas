program TestSquare;

function Square(val: Integer): Integer;
begin
    result := val * val;
end;

begin
    WriteLn('5 squared is ', square(5));
    WriteLn('73 squared is ', square(73));
end.