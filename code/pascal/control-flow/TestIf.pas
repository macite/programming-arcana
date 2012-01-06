program TestIf;

procedure Main();
var
    num, num1: Integer;
begin
    Write('Enter a number: ');
    ReadLn(num);
    
    if num <> 2 then
        WriteLn('Num is not 2!');
    
    Write('Enter another number: ');
    ReadLn(num1);
    
    if (num1 = 2) and (num <> 2) then
        WriteLn('You got the hint... num1 is 2!');
    
    if num > num1 then
        WriteLn('The first number you entered was the larger.')
    else
        WriteLn('The first number you entered was not larger.');
end;

begin
    Main();
end.