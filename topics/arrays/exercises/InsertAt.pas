procedure ????(var data: array of Integer; param3, param4: Integer);
var
    i: Integer;
begin    
    for i := High(data) downto param3 + 1 do
    begin
        data[i] := data[i - 1];
    end;
    
    data[param3] := param4;
end;