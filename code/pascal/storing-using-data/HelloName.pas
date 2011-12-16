program HelloName;

procedure Main();
var
    name: String;
    x, y, z: Integer;
begin
    Write('Enter your name: ');
    ReadLn(name);
    WriteLn('Hello ', name);
    
    Write('Enter three numbers separated by spaces:');
    ReadLn(x, y, z);
    WriteLn('x = ', x, ' y = ', y, ' z = ', z);
end;

begin
    Main();
end.