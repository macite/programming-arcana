type 
    RowPtr = ^Row;
    
    // The Row record/structure. Each row contains an id
    // a kind, and some data (a Row Value).
    Row = record
        id: Integer;
        kind: DataKind;
        data: RowValue;
        next: RowPtr;         // The next Row in the list
    end;

    // The data store is a dynamic linked list of rows, keeping track
    // of the number of rows in the list, and the id for the next row
    DataStore = record
        next_row_id:    Integer; // The id of the row that will be added next
        first_row:      ^Row;    // A pointer to the first row
        last_row:       ^Row;    // A pointer to the last row
    end;


begin
    
end.