program SmallDb;
uses SysUtils;

// =====================
// = Type declarations =
// =====================

type 
    DataKind = {same as original version}
    ColumnValue = {same as original version}
    Row = {same as original version}
    
    // The data store is a dynamic linked list of rows, keeping track
    // of the number of rows in the list, and the id for the next row
    DataStore = record
        nextRowId: Integer;         // The id of the row that will be added next
        rows:      array of Row;    // A dynamic array of rows
    end;
    
    MenuOption = ( ADD_DATA, DELETE_DATA, PRINT_DATA, QUIT );


// =====================================
// = Small DB Functions and Procedures =
// =====================================

function ReadRow(nextId: Integer): Row; {same as original version}
procedure PrintRow(toPrint: row); {same as original version}

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
    rowId: Integer = 0;
begin
    // Allocate the id
    rowId := dbData.nextRowId;
    dbData.nextRowId += 1;
    
    // Store the data
    SetLength(dbData.rows, Length(dbData.rows) + 1);
    dbData.rows[High(dbData.rows)] := ReadRow(rowId);
end;

function IndexOfRowWithID(const dbData: DataStore; rowId: Integer) : Integer;
var
    i: Integer;
begin
    // Loop through each element of the array
    for i := Low(dbData.rows) to High(dbData.rows) do
    begin
        // is this the one we are after?
        if dbData.rows[i].id = rowId then
        begin
            result := i;    // return the index found.
            exit;           // exit the function
        end;
    end;
    
    // Nothing found...
    result := -1;
end;

procedure DeleteRow(var dbData: DataStore);
var
    rowId, i, rowIndex: Integer;
begin
    WriteLn('Please enter id of row to delete: ');
    ReadLn(rowId);
    
    // Get the index of the row
    rowIndex := IndexOfRowWithID(dbData, rowId);
    
    if rowIndex >= 0 then // a row was found to delete...
    begin
        // copy all data past the row, back over the row to delete it
        for i := rowIndex to High(dbData.rows) - 1 do
        begin
            // copy data back one spot in the array (one element at a time)
            dbData.rows[i] := dbData.rows[i+1];
        end;
        
        // resize the array
        SetLength(dbData.rows, Length(dbData.rows) - 1);
    end;
end;

// Print all of the rows from the data store
procedure PrintAllRows(const dbData: DataStore);
var
    i: Integer = 0;
begin
    // For each row in the array
    for i := Low(dbData.rows) to High(dbData.rows) do
    begin
        // Print the row to the Terminal
        PrintRow(dbData.rows[i]);
    end;
end;

// ========
// = Main =
// ========

procedure Main();
var
    opt: MenuOption;
    dbData: DataStore; 
begin
    dbData.nextRowId := 0;
    SetLength(dbData.rows, 0);
    
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