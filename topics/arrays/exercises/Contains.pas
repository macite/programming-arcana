function ????(const data: array of Integer; val: Integer): Boolean;
var
    i: Integer;
begin
    result := False;
    
    for i := Low(data) to High(data) do
    begin
        if data[i] = val then
        begin
            result := True;
            exit;
        end;
    end;
end;