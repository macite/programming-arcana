program ParameterTypes;

procedure TestInByRef(const xIn: Integer);
begin
    WriteLn('I have a reference to the x variable');
    WriteLn('but I can not change it... ', xIn);
end;

procedure TestOutByRef(out xOut: Integer);
begin
    WriteLn('I have a reference to the x variable');
    WriteLn('It does not have a meaningful value');
    WriteLn('but I can store a value in it...');
    xOut := 10;
end;

procedure TestInOutByRef(var xInOut: Integer);
begin
    WriteLn('I have a reference to the x variable');
    WriteLn('I can read its value ', xInOut);
    WriteLn('and I can store a value in it...');
    xInOut := 20;
end;

procedure Main();
var
    x: Integer;
begin
    TestOutByRef(x);
    TestInByRef(x);
    TestInOutByRef(x);
    WriteLn('At the end of main x is ', x);
end;

begin
    Main();
end.