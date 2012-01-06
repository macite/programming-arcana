program StasticsCalculator;
uses SysUtils;

const DATA_SIZE = 10;

// Calculate the Sum of the values in the array
function Sum(const data: array of Double): Double;
var
  i: Integer;
begin
    result := 0;
    
    for i := Low(data) to High(data) do
    begin
        result += data[i];
    end;
end;

// Calculate the Mean of the values in the array
function Mean(const data: array of Double): Double;
begin
    result := Sum(data) / Length(data);
end;

// Find the largest value in the array
function Max(const data: array of Double): Double;
begin
    //todo: add logic here...
    result := 0;
end;

// Find the standard deviation of the values in the array
function Variance(const data: array of Double): Double;
begin
    //todo: add logic here...
    result := 0;
end;

function ReadDouble(prompt: String): Double;
var
  line: String;
begin
    Write(prompt);
    ReadLn(line);
    while not TryStrToFloat(line, result) do
    begin
        WriteLn('Please enter a number.\n');
        Write(prompt);
        ReadLn(line);
    end;
end;

procedure PopulateArray(var data: array of Double);
var
  i: Integer;
begin
    for i := Low(data) to High(data) do
    begin
        data[i] := ReadDouble('Enter value ' + IntToStr(i) + ': ');
    end;
end;

// Implements a statistics calculator. The program reads in values entered by the user
// and then calculates the Sum, Mean, variance, and max
procedure Main();
var
  data: array [0..DATA_SIZE-1] of Double;
begin
    PopulateArray(data);
    
    WriteLn('Calculating statistics...');
    
    WriteLn('Sum:        ', Sum(data):4:2);
    WriteLn('Mean:       ', Mean(data):4:2);
    WriteLn('Variance:   ', variance(data):4:2);
    WriteLn('Max:        ', max(data):4:2);
end;

begin
    Main();
end.