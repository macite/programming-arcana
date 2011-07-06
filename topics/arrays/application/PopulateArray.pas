procedure PopulateArray(var data: array of Double);
var
    i: Integer;
    prompt: String;
begin
    for i := Low(data) to High(data) do
    begin
        prompt := 'Enter value ' + IntToStr(i + 1) + ': ';
        data[i] := ReadDouble(prompt);
    end;
end;
