// Outputs the morse code for the signal 'Calling Anyone' (cq)
program MorseCalling;

// Outputs a dot for a short signal
procedure ShortSignal();
begin
  Write('.');
end;

// Outputs a dash for a long signal
procedure LongSignal();
begin
  Write('-');
end;

procedure SignalC();
begin
  Write(' ');
  LongSignal();
  ShortSignal();
  LongSignal();
  ShortSignal();
end;

procedure SignalQ();
begin
  Write(' ');
  LongSignal();
  LongSignal();
  ShortSignal();
  LongSignal();
end;

procedure Main();
begin
  SignalC();
  SignalQ();
  WriteLn();
end;

begin
    Main();
end.