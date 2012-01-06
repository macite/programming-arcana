program TestRecord;

type 
    Person = record
        name: String;
        age: Integer;
    end;

procedure PrintPerson(const p: Person);
begin
    WriteLn(p.name, ' (aged ', p.age, ')');
end;

function GetPerson(prompt: String): Person;
begin
    WriteLn(prompt);
    
    WriteLn('Enter name: ');
    ReadLn(result.name);
    
    WriteLn('Enter age: ');
    ReadLn(result.age);
end;

procedure Main();
var
    me: Person;
    friends: array [0..1] of Person;
begin
    me.name := 'Fred Smith';
    me.age := 20;
    
    friends[0] := GetPerson('Enter details for a friend.');
    friends[1] := GetPerson('Enter details for another friend.');
    
    PrintPerson(me);
    PrintPerson(friends[0]);
    PrintPerson(friends[1]);
end;

begin
    Main();
end.