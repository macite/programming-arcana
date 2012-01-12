program TextIO;

procedure Main();
var
    output, input: Text;
    toSave: Integer = 20;
    toLoad: Integer;
begin
    Assign(output, 'text.dat');
    Rewrite(output);
    
    WriteLn(output, toSave);
    Close(output);
    
    Assign(input, 'text.dat');
    Reset(input);
    
    ReadLn(input, toLoad);
    Close(input);
    
    WriteLn('Wrote ', toSave, ', read ', toLoad);
end;

begin
    Main();
end.