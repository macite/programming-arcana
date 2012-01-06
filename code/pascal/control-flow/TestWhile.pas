program TestWhile;

function BegForMercy(): Boolean;
var
    ch: Char;
begin
    Write('Beg for mercy? [y/N]: ');
    ReadLn(ch);
    
    result := (ch = 'y') or (ch = 'Y');
end;

procedure Main();
var
    mercy: Boolean = False;
begin
    WriteLn('Before you stands a 12 foot tall Knight...');
    WriteLn('"We are the Knights who say ''Ni''."');
    WriteLn('"I will say Ni to you again if you do not appease us!"');
    
    mercy := BegForMercy();
    
    while not mercy do
    begin
        WriteLn('"Ni!"');
        mercy := BegForMercy();
    end;
    
    WriteLn('"Bring us a Shrubbery!"');
end;

begin
    Main();
end.