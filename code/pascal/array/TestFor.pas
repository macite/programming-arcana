program TestFor;

procedure PrintLine(length: Integer);
var
  i : Integer;
begin
    for i := 1 to length do Write('-');
    WriteLn();
end;

procedure PrintCharacters(text: String);
var
  i : Integer;
begin
    for i := 1 to Length(text) do
    begin
        WriteLn(text[i], ' (ASCII ', Integer(text[i]), ') at index ', i);
    end;
end;

begin
  PrintCharacters('Hello World');
  PrintLine(50);
  PrintCharacters('Fred');
end.
