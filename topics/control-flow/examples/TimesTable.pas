// 
// Program: TimesTable.pas
// Displays the Times Table from 1 x n to 10 x n.
//
program TimesTable;

procedure Main();
var
    number: Integer = 0;
    i: Integer = 0;
begin
    WriteLn('Times Table');
    
    Write('Enter number: ');
    ReadLn(number);
    
    WriteLn('-----------------');
    
    i := 1;
    while i < 10 do
    begin
        WriteLn(' ', i,' x ', number, ' = ', i * number);
        i += 1;
    end;
    
    WriteLn('-----------------');
end;

begin
    Main();
end.