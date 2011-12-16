program SampleWriteLn;
begin
  Write('Hello ');
  Write('World!', LineEnding);
  WriteLn('Single Line Output...');
  WriteLn('Multi', LineEnding, 'Line', LineEnding, 'Output');
  WriteLn('1 + 1 = ', 1 + 1);
  WriteLn(2.4, ' = ', 2.4:10, ' = ', 2.4:4:2);
  WriteLn('It''s a lovely day!');
end.