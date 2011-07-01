function PerformGuess(num_guess, target: Integer): Boolean
var
    guess: Integer;
begin
    
    Write('Guess ', num_guess, ': ');
    Readln(guess);
    
    if target < guess then
    begin
        WriteLn('The number is less than ', guess);
    end
    else
    begin
        if target > guess then
            WriteLn('The number is larger than ', guess)
        else 
            WriteLn('Well done... the number was ', guess);
    end;
    
    result := target = guess;
end;