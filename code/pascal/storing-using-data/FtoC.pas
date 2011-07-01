//
// Program: FtoC.pas
// Converts a temperature from Fahrenheit to Celsius
//
program FtoC;
uses SysUtils;
    
    // Convert the tempF parameter to Celsius and return the result
    function ConvertFtoC(tempF: Double): Double;
    begin
        result := (5/9) * (tempF-32);
    end;
    
    // Read a temperature in Fahrenheit, and output the value
    // converted to Celsius
    procedure Main();
    var
        tempF, tempC: Double;
    begin
        Write('Please enter temperature in Fahrenheit: ');
        ReadLn(tempF);
        tempC := ConvertFtoC(tempF);
        WriteLn('This is ', tempC:4:2, ' in Celsius');
    end;

begin
    Main();
end.