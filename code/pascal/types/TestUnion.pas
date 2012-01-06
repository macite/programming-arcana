program TestUnion;

type 
    MyDataOptions = ( IS_INTEGER, IS_FLOAT, IS_TEXT );
    
    MyUnionType = record
        otherField: String;
        case kind: MyDataOptions of
            IS_INTEGER:  ( asInt:    Integer; );
            IS_FLOAT:    ( asFloat:  Single;  );
            IS_TEXT:     ( asString: String[4];  );
    end;

procedure Main();
var
    data: MyUnionType; // create data of this type
begin
    WriteLn('data is ', SizeOf(data), ' bytes (stores three data values...)');
    
    data.otherField := 'a standard field...';
    data.kind := IS_FLOAT; //should match value stores (programmer managed!)
    data.asFloat := 10.0; //can store as Integer/Float/String based on field used
    
    case kind of
        IS_INTEGER:  WriteLn(data.asInt);
        IS_FLOAT:    WriteLn(data.asFloat:4:2);
        IS_TEXT:     WriteLn(data.asString);
    end;
end;

begin
    Main();
end.