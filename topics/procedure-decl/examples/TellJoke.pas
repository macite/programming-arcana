// Program: TellJoke.pas
// Tell a joke using a number of procedures.
program TellJoke;
uses CRT;

procedure SetupJoke();
begin
  WriteLn('Knock Knock.');
  WriteLn('Who''s there?');
  WriteLn('Boo');
  WriteLn('Boo Who?');
end;

procedure PauseForDramaticEffect();
begin
  Delay(1500);
end;

procedure PunchLine();
begin
  PauseForDramaticEffect();
  WriteLn('Don''t cry its only a joke!');
end;

procedure TellJoke();
begin
  SetupJoke();
  PunchLine();
end;

procedure Ha();
begin
  Write('ha');
end;

procedure HaHa();
begin
  Ha(); Ha();
end;

procedure HeHe();
begin
  Write('hehe');
end;

procedure Ah();
begin
  Write('ah');
end;

procedure HaHaHeHe();
begin
  HaHa();
  HeHe();
end;

procedure Laugh();
begin
  Ah();
  HaHa(); 
  HaHaHeHe();
  PauseForDramaticEffect();
  HeHe(); 
end;

procedure StopLaughing();
begin
  WriteLn();
  Ah();
  PauseForDramaticEffect();
  WriteLn();
  WriteLn('Yes, very funny :)');
end;

procedure Main();
begin
  TellJoke();
  Laugh();
  StopLaughing();
end;

begin
  Main();
end.