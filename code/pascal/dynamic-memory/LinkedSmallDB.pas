program SmallDb;
uses SysUtils;

type 
    // The Data Kind enumeration indicates the kind of data stored in a row, matches
    // the options available in the Column Value union.
    DataKind = ( INT_VAL, DBL_VAL, TXT_VAL );
    
    ColumnValue = record // The Column Value union.
        case kind: DataKind of   // Has a kind field (as well as one of)
            INT_VAL: ( intVal: Integer;     );  // Stores either an Integer,
            // todo: add double as an option 
            TXT_VAL: ( txtVal: String[7];   );  // 7 characters. (8 bytes)
    end;
    
    // A Pointer to a row (Row must be in same type decl part)
    RowPtr = ^Row;
    
    // The Row record/structure. Each row contains an id
    // and some data (a Column Value).
    Row = record
        id: Integer;
        data: ColumnValue;
        next: RowPtr;         // The next Row in the list
    end;

    // The data store is a dynamic linked list of rows, keeping track
    // of the number of rows in the list, and the id for the next row
    DataStore = record
        nextRowId:  Integer;    // The id of the row that will be added next
        firstRow:   RowPtr;     // A pointer to the first row
        lastRow:    RowPtr;     // A pointer to the first row
    end;
    
    MenuOption = ( ADD_DATA, DELETE_DATA, PRINT_DATA, QUIT );

// Read a row in from the user and return it. 
function ReadRow(nextId: Integer): Row;
var
    line: String = '';
begin
    //store the id
    result.id := nextId;    // The nextId is the id number for the newly created row
    result.next := nil;     // Nothing after this row... at this point
    
    Write('Enter value: ');
    ReadLn(line);       // Read the value from the user into the line
    
    // test int first, read the Integer from the line, and store in row
    if TryStrToInt(line, result.data.intVal) then
    begin
        result.data.kind := INT_VAL;    // was an Integer, so set kind to INT_VAL
    end
    // else if TryStrToFloat(line, result.data.dblVal) then // test dbl
    //     //todo: add double as an option...
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

function GetMenuOption() : MenuOption;
var
    input: Integer = 0;
begin
    WriteLn('=========================');
    WriteLn('| Small DB              |');
    WriteLn('=========================');
    WriteLn(' 1: Add Data');
    WriteLn(' 2: Print Data');
    WriteLn(' 3: Delete Data');
    WriteLn(' 4: Quit');
    WriteLn('=========================');
    
    Write('Choose Option: ');
    ReadLn(input);
    
    while (input < 1) or (input > 4) do
    begin
        WriteLn('Please enter a value between 1 and 4.');
        Write('Choose Option: ');
        ReadLn(input)
    end;
    
    case input of
        1: result := ADD_DATA;
        2: result := PRINT_DATA;
        3: result := DELETE_DATA;
        4: result := QUIT;
        else result := QUIT;
    end;
end;

procedure AddRow(var dbData: DataStore);
var
    rowID: Integer = 0;
    newRow: RowPtr;
begin    
    // Allocate the id
    rowID := dbData.nextRowId;
    dbData.nextRowID += 1;
    
    // Allocate space on the heap for the new row
    New(newRow);
    
    newRow^ := ReadRow(rowID);
    newRow^.next := nil; // there is nothing after this row
    
    if dbData.lastRow = nil then
    begin
        // The data store must be empty, new row is
        // the start and the end.
        dbData.firstRow := newRow;
    end
    else
    begin
        // The row come after the last row, so change then
        // current last row's next
        dbData.lastRow^.next := newRow;
    end;
    
    // The new row is the last row in the list
    dbData.lastRow := newRow;
end;

procedure DeleteRow(var dbData: DataStore);
var
    rowId: Integer;
    current, next, prev: RowPtr;
begin    
    WriteLn('Please enter id of row to delete: ');
    ReadLn(rowId);
    
    current := dbData.firstRow;
    prev := nil; // There is no previous for the first row
    
    while (current <> nil) and (current^.id <> rowId) do
    begin
        prev := current;            // previous, is now current
        current := current^.next;   // current is... one after current
    end;
    
    if current = nil then exit; // No row found
    
    next := current^.next;          // the one after the node to delete
    
    if prev = nil then
    begin
        // Deleting the first row, so change the start
        dbData.firstRow := next;
    end
    else
    begin
        // Skip the row that is about to be deleted
        prev^.next := next; // the one before points to the one after
    end;
    
    if current = dbData.lastRow then
    begin
        // Last row was deleted, so update the last row of the data store
        dbData.lastRow := prev;
    end;
    
    // Now free the current row
    Dispose(current);
end;

// Print all of the rows from the data store
procedure PrintAllRows(const dbData: DataStore);
var
    current: RowPtr;
begin
    current := dbData.firstRow;     // current is the first row
    
    while current <> nil do         // While there is a current node
    begin
        PrintRow(current^);         // Print the row to the Terminal
        current := current^.next;
    end;
end;

// ========
// = Main =
// ========

// Entry point
procedure Main();
var
    opt: MenuOption;
    dbData: DataStore; 
begin
    dbData.nextRowId := 0;
    dbData.firstRow := nil;
    dbData.lastRow := nil;
    
    repeat
        opt := GetMenuOption();
        
        case opt of
            ADD_DATA:       AddRow(dbData);
            DELETE_DATA:    DeleteRow(dbData);
            PRINT_DATA:     PrintAllRows(dbData);
            QUIT:           WriteLn('Bye.');
        end;
    until opt = QUIT;
end;

begin
    Main();
end.