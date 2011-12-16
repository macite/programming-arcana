//  This program demonstrates some variable declarations.
program VariableTest;

const PI = 3.1415;
var 
    globalFloat: Single = 12.3;
    globalInt: Integer = 73;

procedure Test(paramInt: Integer; paramFloat: Single);
var
    myLocal: Integer = 37;
    anotherLocal: Integer = 42;
begin
    WriteLn('my local int = ', myLocal, ', anotherLocal = ', anotherLocal);
    WriteLn('param int = ', paramInt, ', param int2 = ', paramFloat:4:2);
    WriteLn('globals are ', globalFloat, ' and ', globalInt);
end;

procedure Main();
var
    localInt: Integer;
begin
    localInt := 21;
    
    Test(localInt, PI * localInt * localInt);
    
    WriteLn('local int = ', localInt);
    WriteLn('globals are ', globalFloat, ' and ', globalInt);
    WriteLn('PI is a constant with value ', PI);
end;

begin
    Main();
end.