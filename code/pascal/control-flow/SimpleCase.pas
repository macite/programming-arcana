program SimpleCase;

procedure Main();
var
    ch: Char;
begin
    Write('Enter a character: ');
    ReadLn(ch);
    
    case ch of
      'a', 'b':           WriteLn('a or b');
      'c', 'e':           WriteLn('c or e');
      'd':                WriteLn('d');
      'f'..'z', 'F'..'Z': WriteLn('f to z or F to Z')
      else                WriteLn('Something else...');
    end;
end;

begin
  Main();
end.