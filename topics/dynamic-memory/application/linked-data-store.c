// The Row record/structure. Each row contains an id
// a kind, and some data (a Column Value).
typedef struct row_struct {
        int                 id;
        data_kind           kind;
        column_value        data;
        struct row_struct   *next;  // The next Row in the list
    } row;

// The data store is a dynamic linked list of rows, keeping track
// of the number of rows in the list, and the id for the next row
typedef struct {
    int     next_row_id;    // The id of the row that will be added next
    row     *first_row;      // A pointer to the first row
    row     *last_row;       // A pointer to the last row
} data_store;
