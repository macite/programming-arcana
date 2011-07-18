procedure DeleteARow(var dbData: DataStore);
var
    rowId: Integer;
    current, next, prev: ^Row;
begin
    Write('Please enter id of row to delete: ');
    ReadLn(rowId);
    
    current := dbData.FirstRow; // Start searching for the row to delete
    prev := nil; // There is no previous for the first row
    
    while (current <> nil) and (current^.id <> rowId) do
    begin
        prev := current;             // Old current is new prev
        current := current^.next;    // New current is current's next
    end;
    
    next := current^.next;  // get the new "next" Row
    prev^.next := next;     // skip the current Row
    Dispose(current);       // Release the memory
end;
