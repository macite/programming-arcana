program TestCompound;

procedure Main();
var
  ch: Char;
begin
    Write('Enter the forest? [y/N]: ');
    ReadLn(ch);
    
    if (ch = 'y') or (ch = 'Y') then
    begin
        WriteLn('We are the Knights who say "Ni"!'); 
        WriteLn('We are the keepers of the sacred words: ');
        WriteLn('"Ni", "Peng", and "Neee-wom"!'); 
        WriteLn('The Knights Who Say "Ni" demand a sacrifice.'); 
        WriteLn('We want... a shrubbery!');
    end
    else
    begin
        WriteLn('Greetings Sir Robin.');
    end;
end;

