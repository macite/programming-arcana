program DynamicArrayExample;

procedure Main();
var
    line: String;
    names: array of String;
    i: Integer;
begin
    SetLength(names, 0);
    
    WriteLn('Enter a list of names, ending with an empty value.');
    ReadLn(line);
    
    while Length(line) > 0 do
    begin
        SetLength(names, Length(names) + 1);    // Increase array size
        names[High(names)] := line;             // Set last name
        ReadLn(line);
    end;
    
    for i := Low(names) to High(names) do
    begin
        WriteLn(names[i]);
    end;
end;

begin
    Main();
end.