// This program is an implementation of the 'guess that number'
// game. The computer randomly chooses a number and the player
// attempts to guess it. (It should never take more than 7 guesses)
program GuessThatNumber;

const
  MAX_NUMBER  = 100;
  MAX_GUESSES = 7;

// Print a line onto the Terminal.
procedure PrintLine(len: Integer);
var
  i: Integer = 0;
begin
    while ( i < len ) do
    begin
        Write('-');
        i += 1;
    end;
    WriteLn();
end;

// Perform the steps for the guess. Reads the value entered by the user,
// outputs a message, and then returns true if the got it otherwise it returns
// false.
function PerformGuess(numGuess, target: Integer): Boolean;
var
  guess: Integer;
begin
    Write('Guess ', numGuess, ': ');
    ReadLn(guess);
    
    if target < guess then WriteLn('The number is less than ', guess)
    else if target > guess then WriteLn('The number is larger than ', guess)
    else WriteLn('Well done... the number was ', guess);
    
    result := target = guess;   // return true when "target equals guess"
end;

// Implements a simple guessing game. The program generates
// a random number, and the player tries to guess it.
procedure PlayGame();
var
  myNumber, numGuess: Integer;
  gotIt: Boolean = False;
begin
    myNumber := Random(MAX_NUMBER) + 1;
    numGuess := 0; //Keep track of the number of guesses
    
    WriteLn('I am thinking of a number between 1 and ', MAX_NUMBER);
    WriteLn();
    
    repeat
        numGuess += 1;
        gotIt := PerformGuess(numGuess, myNumber);
    until (numGuess > MAX_GUESSES) or gotIt;
    
    if not gotIt then
    begin
        WriteLn('You ran out of guesses... the number was ', myNumber);
    end;
end;

// Loops the guessing game until the user decided to quite.
procedure Main();
var
  again: Char;
begin
    Randomize();
    
    repeat
        PlayGame();
        WriteLn();
        PrintLine(50);
        WriteLn('Do you want to play again [Y/n]? ');
        ReadLn(again);
    until (again = 'n') or (again = 'N');
    
    WriteLn('Bye');
end;

begin
  Main();
end.
