typedef struct {
    int next_row_id;    // The id of the row that will be added next
    int row_count;      // The number of rows in the array
    row *rows;          // A pointer to the rows in memory
} data_store;
