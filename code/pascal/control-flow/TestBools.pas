program TestBools;

procedure PrintBoolean(value: Boolean);
begin
    if value then   // if value is true then...
        Write('true')
    else 
        Write('false');
end;

// Is v1 at least double v2
function AtLeastDouble(v1, v2: Integer): Boolean;
begin
    result := v1 >= 2 * v2;
end;

procedure Main();
var
  test: Boolean;
  num: Integer;
begin
    Write('Enter a number: ');
    ReadLn(num);
    
    test := AtLeastDouble(num, 5);
    Write(num, ' is at least double 5 is ');
    PrintBoolean(test);
    WriteLn();
end;

begin
  Main();
end.