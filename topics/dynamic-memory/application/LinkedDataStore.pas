type 
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
        nextRowID:    Integer; // The id of the row that will be added next
        firstRow:      ^Row;    // A pointer to the first row
        lastRow:       ^Row;    // A pointer to the last row
    end;
