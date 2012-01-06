program TestArrayPassing;

procedure TestPassInByVal(data: array of Integer);
begin
    WriteLn('Can read and change (local copy of) data -> ', 
            data[Low(data)], ' to ', data[High(data)]);
    data[0] := -1000;
    WriteLn('Local copy now -> ', data[Low(data)], ' to ', data[High(data)]);
end;

//----------------------------------------------------------------

procedure TestPassInByRef(const data: array of Integer);
begin
    WriteLn('Can only read from data -> ', 
            data[Low(data)], ' to ', data[High(data)]);
end;

//----------------------------------------------------------------

procedure TestPassInOutByRef(var data: array of Integer);
begin
    WriteLn('Can read and change data');
    data[0] := data[0] + 1;                  //increment first
    data[High(data)] := data[High(data)] * 2;    //double last
end;

//----------------------------------------------------------------

procedure Main();
var
    myData: array [0..2] of Integer = (1, 2, 3);
    otherData: array [0..1] of Integer = (7, 281);
begin
    TestPassInByVal(myData);
    TestPassInByRef(myData);
    
    TestPassInOutByRef(myData);
    TestPassInByRef(myData);
    
    TestPassInByRef(otherData);
end;

begin
    Main();
end.
