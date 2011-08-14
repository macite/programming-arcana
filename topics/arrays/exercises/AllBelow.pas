function ????(const data: array of Integer; max: Integer): Boolean;
var
    i: Integer;
begin
    result := true;

    for i := Low(data) to High(data) do
    begin
        if data[i] > max then
        begin
          result := false;
          break;
        end;
    end;
end;