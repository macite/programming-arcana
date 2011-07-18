type DataStore = record
        NextRowId: Integer; // The id of the row that will be added next
        Rows: array of Row; // The dynamic array of rows
    end;    