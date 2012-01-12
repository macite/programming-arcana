program SmallDb;
uses SysUtils;

// The number of elements in the row array
const DB_SIZE = 3;

type
    // The Data Kind enumeration indicates the kind of data
    // stored in a row, matches the options available in the
    // Column Value union.
    DataKind = ( INT_VAL, DBL_VAL, TXT_VAL );
    
    ColumnValue = record    // The Row record/structure. Each row contains an id
        case kind: DataKind of
            INT_VAL: ( intVal: Integer;     );  // The Column Value union. Stores either an Integer, a
            DBL_VAL: ( dblVal: Double;      );  // Double or 
            TXT_VAL: ( txtVal: String[7];   );  // 7 characters. (8 bytes)
    end;
    
    Row = record    // The Row record/structure. Each row contains an id
        id:     Integer;
        data:   ColumnValue;
    end;

// Display the intro message.
procedure ShowIntro();
begin
    WriteLn('-----------------------');
    WriteLn('  Welcome to Small DB');
    WriteLn('-----------------------');
    WriteLn('Please enter ', DB_SIZE, ' values.');
    WriteLn('They can be text');
    WriteLn('or numbers.');
end;

// Read a row in from the user and return it. The next_id
// is the id number for the newly created row.
function ReadRow(nextId: Integer): Row;
var
    line: String = '';
begin
    //store the id
    result.id := nextId;
    
    // Read the value from the user into the line
    Write('Enter value: ');
    ReadLn(line);
    
    // test int first, read the Integer from the line, and store in row
    if TryStrToInt(line, result.data.intVal) then
    begin
        // was an Integer, so store the kind in the row
        result.data.kind := INT_VAL;
    end
    else if TryStrToFloat(line, result.data.dblVal) then // test dbl
    begin
        // was a Double, so store the kind in the row
        result.data.kind := DBL_VAL;
    end
    else
    begin
        // is not Integer or Double, so its text. Copy the text into the row
        result.data.txtVal := line;
        // store the kind in the row
        result.data.kind := TXT_VAL;
    end;
    
    WriteLn('Stored in row with id ', result.Id);
end;

// Print the row to the Terminal
procedure PrintRow(toPrint: row);
begin
    // Print the row's id
    WriteLn('Row with id ', toPrint.Id);
    
    // Branch based on the kind, and output the data
    case (toPrint.data.kind) of
        INT_VAL:
            WriteLn(' has Integer ', toPrint.data.intVal);
        DBL_VAL: 
            WriteLn(' has Double ', toPrint.data.dblVal:4:2);
        TXT_VAL:
            WriteLn(' has text ', toPrint.data.txtVal);
        else 
            WriteLn(' has an unknown value ');
    end;
end;

// Entry point
procedure Main();
var
    dbData: array[0..DB_SIZE - 1] of Row;   // Create array or Column Values
    i: Integer;
begin
    ShowIntro();
    
    // For each row in the array
    for i := Low(dbData) to High(dbData) do
        // Read the current row's value from the Terminal
        dbData[i] := ReadRow(i);
    
    // For each row in the array
    for i := Low(dbData) to High(dbData) do
        // Print the row to the Terminal
        PrintRow(dbData[i]);
end;

begin
    Main();
end.