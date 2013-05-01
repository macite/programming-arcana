function ReadDouble(const prompt: String): Double;
var
    temp: String;
begin
    Write(prompt);
    ReadLn(temp);   // Read the input as a string
    
    while not TryStrToFloat(temp, result) do
    begin
        // Convert failed, as input was not a number
        WriteLn('Please enter a number.');
        
        Write(prompt);
        ReadLn(temp);
    end;
end;