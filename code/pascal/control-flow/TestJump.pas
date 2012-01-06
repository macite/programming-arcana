program TestJump;

procedure Main();
var
  i: Integer = 0;
  ch: Char;
begin
    while i < 10000000 do
    begin
        i := i + 1;
        
        if i mod 2 = 0 then continue; // Skip all even numbers
        
        WriteLn('At ', i);
        
        Write('Quit? [y/N]: ');
        ReadLn(ch);
        
        if (ch = 'y') or (ch = 'Y') then
        begin
            WriteLn('Quitting loop...');
            break;  // End the loop
        end;
    end;
    WriteLn('Bye...');
    exit; // Exit function
    
    // Code cannot be reached!
    WriteLn('This will never be printed!');
end;

begin
  Main();
end.