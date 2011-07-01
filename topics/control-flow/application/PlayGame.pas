procedure PlayGame();
var
    myNumber, guessNum: Integer;
    gotIt: Boolean;
begin
    myNumber := Random(MAX_NUMBER) + 1;
    guessNum := 0; //Keep track of the number of guesses
    
    WriteLn('I am thinking of a number between 1 and ', MAX_NUMBER);
    
    repeat
        guessNum += 1;
        gotIt := PerformGuess(guessNum, myNumber);
    until (guessNum >= MAX_GUESSES) or (gotIt);
    
    if not gotIt then
    begin
        WriteLn('You ran out of guesses... the number was ', myNumber);
    end;
end;
