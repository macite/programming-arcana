function Sum(const data: array of Double): Double;
var
    i: Integer;
    total: Double;
begin
    total := 0;
    
    for i := Low(data) to High(data) do
    begin
        total += data[i];
    end;
    
    result := total;
end;