// 
// Program: TimesTable.pas
// Displays the Times Table from 1 x n to 10 x n.
//
program TimesTable;

procedure Main();
var
    number: Integer;
begin
    WriteLn('Times Table');
    
    Write('Enter number: ');
    ReadLn(number);
    
    WriteLn('-----------------');
    WriteLn(' 1 x ', number, ' = ', 1 * number);
    WriteLn(' 2 x ', number, ' = ', 2 * number);
    WriteLn(' 3 x ', number, ' = ', 3 * number);
    WriteLn(' 4 x ', number, ' = ', 4 * number);
    WriteLn(' 5 x ', number, ' = ', 5 * number);
    WriteLn(' 6 x ', number, ' = ', 6 * number);
    WriteLn(' 7 x ', number, ' = ', 7 * number);
    WriteLn(' 8 x ', number, ' = ', 8 * number);
    WriteLn(' 9 x ', number, ' = ', 9 * number);
    WriteLn('10 x ', number, ' = ', 10 * number);
    WriteLn('-----------------');
end;

begin
    Main();
end.