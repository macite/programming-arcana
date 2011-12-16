program SayHelloProc; // Outputs 'Hello' messages to the Terminal.

procedure SayHello();
begin
    WriteLn('Hello...');
end;

procedure SayIsAnyoneThere();
begin
    WriteLn('Is anyone there?');
end;

procedure Main();
begin
    SayHello();
    SayHello();
    SayIsAnyoneThere();
end;

begin
    Main();
end.