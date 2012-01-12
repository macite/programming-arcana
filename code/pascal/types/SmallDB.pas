program SmallDb;
uses SysUtils;

// The number of elements in the row array
const DB_SIZE = 3;

type
    // The Data Kind enumeration indicates the kind of data stored in a row, matches
    // the options available in the Column Value union.
    DataKind = ( INT_VAL, DBL_VAL, TXT_VAL );
    
    ColumnValue = record // The Column Value union.
        case kind: DataKind of
            INT_VAL: ( intVal: Integer;     );  // Stores either an Integer,
            // todo: add double as an option 
            TXT_VAL: ( txtVal: String[7];   );  // 7 characters. (8 bytes)
    end;
    
    Row = record                // The Row record/structure.
        id:     Integer;        // Each row contains an id
        data:   ColumnValue;    // and a single column
    end;

procedure ShowIntro();  // Display the intro message.
begin
    WriteLn('-----------------------');
    WriteLn('  Welcome to Small DB');
    WriteLn('-----------------------');
    WriteLn('Please enter ', DB_SIZE, ' values.');
    WriteLn('They can be text');
    WriteLn('or numbers.');
end;

// Read a row in from the user and return it. 
function ReadRow(nextId: Integer): Row;
var
    line: String = '';
begin
    //store the id
    result.id := nextId;    // The nextId is the id number for the newly created row
    
    Write('Enter value: ');
    ReadLn(line);       // Read the value from the user into the line
    
    // test int first, read the Integer from the line, and store in row
    if TryStrToInt(line, result.data.intVal) then
    begin
        result.data.kind := INT_VAL;    // was an Integer, so set kind to INT_VAL
    end
    else if TryStrToFloat(line, result.data.dblVal) then // test dbl
        //todo: add double as an option...
    else
    begin
        // is not Integer or Double, so its text. Copy the text into the row
        result.data.txtVal := line;
        result.data.kind := TXT_VAL;    // was text, so set kind to TXT_VAL
    end;
    
    WriteLn('Stored in row with id ', result.Id);
end;

// Print the row to the Terminal
procedure PrintRow(toPrint: row);
begin
    WriteLn('Row with id ', toPrint.Id);    // Print the row's id
    
    // Branch based on the kind, and output the data
    case (toPrint.data.kind) of
        INT_VAL:    WriteLn(' has Integer ', toPrint.data.intVal);
        // DBL_VAL: todo: add double as an option
        TXT_VAL:    WriteLn(' has text ', toPrint.data.txtVal);
        else        WriteLn(' has an unknown value ');
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