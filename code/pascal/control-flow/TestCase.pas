program TestCase;

function ReadMenuOption(): Integer;
begin    
    WriteLn('1: Greet the Knights of Ni');
    WriteLn('2: Offer Knights a shrubbery');
    WriteLn('3: Refuse to cut down tree with Herring');
    WriteLn('4: Tell them all about it');
    
    Write('Option: ');
    ReadLn(result);
end;

procedure Main();
var
    option: Integer;
begin
    option := ReadMenuOption();
    
    case option of
        1: WriteLn('We say Ni to you!'); 
        2: WriteLn('Cut down the mightiest tree... with a Herring!'); 
        3: WriteLn('Oh please...'); 
        4: WriteLn('Argh... dont say that word!') 
        else WriteLn('Please enter a value between 1 and 4');
    end;
end;

begin
    Main();
end.