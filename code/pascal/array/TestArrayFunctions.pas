procedure Main();
var
    i: Integer;
    myList: array [0..5] of Integer;
    otherArr: array [7..23] of Integer;
begin
    WriteLn('myList has ', Length(myList), ' elements.');
    WriteLn('Its first index is ', Low(myList), ', its last is ', High(myList));
    WriteLn('for i := Low(myList) to High(myList) do ...');
    
    for i := Low(myList) to High(myList) do 
        WriteLn('process myList[', i, ']');
    
    WriteLn('otherArr has ', Length(otherArr), ' elements.');
    WriteLn('from ', Low(otherArr), ', to ', High(otherArr));
end;

begin
    Main();
end.