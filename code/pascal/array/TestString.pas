program TestString;

procedure Main();
var
    sampleText, name: String;
    word: String[9];
begin
    sampleText := 'Hello World';
    
    WriteLn(sampleText, ' has ', Length(sampleText) ,' characters');
    
    WriteLn('Enter a word (upto 9 characters long): ');
    ReadLn(word);
    WriteLn('You entered ', word);
    
    WriteLn('Enter your full name: ');
    ReadLn(name);
    WriteLn('Welcome ', name);
    
    if name = 'Fred Smith' then
    begin
        WriteLn('Wow, you have the same name as used in the text!');
    end;
end;

begin
    Main();
end.