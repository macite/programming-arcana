program SmallDb;
uses SysUtils;

type 
    MenuOption = {same as original version}
    DataKind = {same as original version}
    ColumnValue = {same as original version}
    
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

// Read a row in from the user and return it. 
function ReadRow(nextId: Integer): Row;
var
    line: String = '';
begin
    //store the id
    result.id := nextId;    // The nextId is the id number for the newly created row
    result.next := nil;     // Nothing after this row... at this point
    
    {Remainder is the same as original version...}
end;

procedure PrintRow(toPrint: row); {same as original version}
function GetMenuOption() : MenuOption; {same as dynamic array version}

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
    
    {Remainder is the same as dynamic array version...}
end;

begin
    Main();
end.