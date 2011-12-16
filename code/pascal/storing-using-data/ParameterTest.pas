program ParameterTest;

procedure PrintEquation(m: Integer; x: Double; c: Integer);
begin
    WriteLn(m, ' x ', x:4.2, ' + ', c, ' = ', m * x + c);
end;

begin
    PrintEquation(2, 5.1, 3);
    PrintEquation(7, 2.74, -8);
end.