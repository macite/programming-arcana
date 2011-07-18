procedure PrintAllRows(const dbData: DataStore);
var
    current: ^Row;
begin
    current := dbData^.firstRow;
    
    // While there is a current node
    while current <> nil do
    begin
        // Print the row to the Terminal
        PrintRow(current^);        // Follow the pointer, pass the value
        current := current^.next;  // Move to next Row
    end;
end;
