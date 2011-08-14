function Median (const data: array of Integer): Integer;
var
    startIdx, endIdx: Integer;
begin
    startIdx := Low(data);
    endIdx := High(data);
    
    while (startIdx <> endIdx) and (startIdx < endIdx) do
    begin
        startIdx := startIdx + 1;
        endIdx := endIdx - 1;
    end;
    
    if startIdx = endIdx then result := data[startIdx]
    else result := data[startIdx] + data[endIdx] / 2;
end;