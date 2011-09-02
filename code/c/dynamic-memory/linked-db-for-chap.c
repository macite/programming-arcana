/* Program: small-db-2.c */

/* Includes are the same as original version */

// =====================
// = Type declarations =
// =====================

typedef enum { /* same as array version */ } menu_option;
typedef union { /* same as original version */ } row_value;
typedef enum { /* same as original version */ } data_kind;

// The Row record/structure. Each row contains an id
// a kind, and some data (a Row Value).
typedef struct row_struct {
        int                 id;
        data_kind           kind;
        row_value           data;
        struct row_struct   *next;  // Points to the next row
    } row;

// The data store is a dynamic linked list of rows, keeping track
// of the number of rows in the list, and the id for the next row
typedef struct {
        int     next_row_id;    // The id of the row that will be added next
        row     *first_row;      // A pointer to the first row
        row     *last_row;       // A pointer to the last row
    } data_store;

// ====================================
// = General Functions and Procedures =
// ====================================
void trim(char* text, int n) { /* same as original version */ }
bool is_integer(const char* text) { /* same as original version */ }
bool is_double(const char* text) { /* same as original version */ }
void clear_input()  { /* same as original version */ }

// =====================================
// = Small DB Functions and Procedures =
// =====================================

// Read a row in from the user and return it. The next_id
// is the id number for the newly created row.
row read_row(int next_id)
{
    char line[16] = "", temp[2];
    row result = {0, UNK_VAL, {0}, NULL};   // need to initialise next to point to NULL
    
    /* The remainder of this function is the same as the original version */
}

void print_row(row to_print)  { /* same as original version */ }
menu_option get_menu_option()  { /* same as array version */ }

// Add a row to the data store
void add_a_row(data_store *db_data)
{
    int row_id = 0;
    row *new_row;
    
    if (db_data == NULL) return;
    
    // Allocate the id
    row_id = db_data->next_row_id;
    db_data->next_row_id++;
    
    // Allocate space on the heap for the new row
    new_row = (row *)malloc(sizeof(row));
    
    *new_row = read_row(row_id);
    new_row->next = NULL; // there is nothing after this row
    
    if (db_data->last_row == NULL)
    {
        // The data store must be empty, new row is
        // the start and the end.
        db_data->first_row = new_row;
    }
    else
    {
        // The row come after the last row, so change then
        // current last row's next
        db_data->last_row->next = new_row;
    }
    
    // The new row is the last row in the list
    db_data->last_row = new_row;
}

// Delete a row from the data store
void delete_a_row(data_store *db_data)
{
    int row_id;
    row *current, *next, *prev;
    
    if (db_data == NULL) return;
    
    printf("Please enter id of row to delete: ");
    scanf("%d", &row_id);
    
    current = db_data->first_row;
    prev = NULL; // There is no previous for the first row
    
    while(current != NULL && current->id != row_id)
    {
        prev = current;
        current = current->next;
    }
    
    if ( current == NULL ) return; // No row found
    
    next = current->next;
    
    if ( prev == NULL )
    {
        // Deleting the first row, so change the start
        db_data->first_row = next;
    }
    else
    {
        // Skip the row that is about to be deleted
        prev->next = next;
    }
    
    if ( current == db_data->last_row )
    {
        // Last row was deleted, so update the last row of the data store
        db_data->last_row = prev;
    }
    
    // Now free the current row
    free(current);
}

// Print all of the rows from the data store
void print_all_rows(const data_store *db_data)
{
    row *current;
    
    if (db_data == NULL) return;
    
    current = db_data->first_row;       // current is the first row
    
    while(current != NULL)              // While there is a current node
    {
        print_row(*current);            // Print the row to the Terminal
        current = current->next;
    }
}

// ========
// = Main =
// ========

// Entry point
int main()
{
    menu_option opt;
    data_store db_data = {0, NULL, NULL}; // id, first, last
    
    /* The remainder of this function is the same as the array version */
}