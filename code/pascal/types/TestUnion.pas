program TestUnion;

type MyUnionType = record
        case Boolean of
            False: (asInt: Integer;);
            True: (asFloat: Single;);
    end;

procedure Main();
var
    data: MyUnionType; // create data of this type
begin
    data.asFloat := 10;
    
    WriteLn(data.asFloat:4:2);
    WriteLn(data.asInt);
    
end;

begin
    Main();
end.