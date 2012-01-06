program TestArray;

procedure Main();
var
    data: array [0..2] of Integer = (0, 1, 2);
    data1: array [0..2] of Integer;
    i: Integer;
begin
    data1 := data;      // copy from data into data1...
    data[0] := 1;
    for i := 0 to 2 do
    begin
        WriteLn('data[', i, ']  = ', data[i]);
        WriteLn('data1[', i, '] = ', data1[i]);
    end;
end;

begin
    Main();
end.