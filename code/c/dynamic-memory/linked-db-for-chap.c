/* Program: small-db-2.c */
#include <stdio.h>
#include <strings.h>
#include <stdbool.h>
#include <stdlib.h>
#include <errno.h>
#include <limits.h>

// =====================
// = Type declarations =
// =====================

// The Row Value union. Stores either an integer, a
// double or 8 (7 + 1) characters.
typedef union { /* same as array version */ } row_value;

// The Data Kind enumeration indicates the kind of data
// stored in a row, matches the options available in the
// Row Value union.
typedef enum { /* same as array version */ } data_kind;

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

// The user can choose to add, delete, or print data or to quit
typedef enum { /* same as array version */ } menu_option;

// ====================================
// = General Functions and Procedures =
// ====================================

// Trim spaces from the start/end of a string (in place)
// This is passed the string to trim, and the number of characters it contains
void trim(char* text, int n) { /* same as array version */ }

// Test if the passed in text refers to an integer
bool is_integer(const char* text) { /* same as array version */ }

// Test if the passed in text refers to a double
bool is_double(const char* text) { /* same as array version */ }

void clear_input()  { /* same as array version */ }

// =====================================
// = Small DB Functions and Procedures =
// =====================================

// Read a row in from the user and return it. The next_id
// is the id number for the newly created row.
row read_row(int next_id)
{
    char line[16] = "", temp[2];
    row result = {0, UNK_VAL, {0}, NULL};
    
    /* The remainder of this function is the same as the array version */
}

// Print the row to the Terminal
void print_row(row to_print)  { /* same as array version */ }

menu_option get_menu_option()  { /* same as array version */ }

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

void delete_a_row(data_store *db_data)
{
    int row_id;
    row *current;
    row *next;
    row *prev;
    
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

void print_all_rows(const data_store *db_data)
{
    row *current;
    
    if (db_data == NULL) return;
    
    current = db_data->first_row;
    
    // While there is a current node
    while(current != NULL)
    {
        // Print the row to the Terminal
        print_row(*current);
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