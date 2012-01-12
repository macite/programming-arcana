procedure Main();
var
    message: String = 'Hello World';
    inMessage: String;
    val: Integer = 127;
    inVal: Integer;
    output, input: Text;
begin    
    Assign(output, 'test.txt');
    Rewrite(output); // Change to write mode, deleting old contents
    WriteLn(output, message);
    WriteLn(output, val);
    Close(output);
    
    Assign(input, 'test.txt');
    Reset(input); // Reset to read from the start of the file
    
    ReadLn(input, inMessage); // read first 5 characters of the message
    ReadLn(input, inVal); // read the number
    
    Close(input);
    
    WriteLn('Read ', inMessage, ' & ', inVal);
end;

begin
    Main();
end.