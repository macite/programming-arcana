/* Program: small-db-2.c - array version */
#include <stdio.h>
#include <strings.h>
#include <stdbool.h>
#include <stdlib.h>
#include <errno.h>
#include <limits.h>

// =====================
// = Type declarations =
// =====================

// The user can choose to add, delete, or print data or to quit
typedef enum {
    ADD_DATA,
    DELETE_DATA,
    PRINT_DATA,
    QUIT
} menu_option;

typedef union { /* same as original version */ } column_value;
typedef enum { /* same as original version */ } data_kind;
typedef struct { /* same as original version */ } row;

// The data store is a dynamic array of rows, keeping track
// of the number of rows in the array, and the id for the next row
typedef struct {
    int next_row_id;    // The id of the row that will be added next
    int row_count;      // The number of rows in the array
    row *rows;          // A pointer to the rows in memory
} data_store;

// ====================================
// = General Functions and Procedures =
// ====================================
void trim(char* text, int n) { /* same as original version */ }
bool is_integer(const char* text) { /* same as original version */ }
bool is_double(const char* text) { /* same as original version */ }
void clear_input() { /* same as original version */ }

// =====================================
// = Small DB Functions and Procedures =
// =====================================
row read_row(int next_id) { /* same as original version */ }
void print_row(row to_print) { /* same as original version */ }
menu_option get_menu_option() { /* same as original version */ }

// Add a row to the data store
void add_a_row(data_store *db_data)
{
    int row_id = 0;
    
    if (db_data == NULL) return;
    
    // Allocate the id
    row_id = db_data->next_row_id;
    db_data->next_row_id++;
    
    // Store the data - using realloc to allocate space for the rows
    db_data->row_count++;
    db_data->rows = (row *) realloc(db_data->rows, sizeof(row) * db_data->row_count);
    db_data->rows[db_data->row_count - 1] = read_row(row_id); // last idx = n - 1
}

// Get the array index of the row with the indicated ID
int idx_of_row_with_id(const data_store *db_data, int row_id)
{
    int i;
    
    if (db_data == NULL) return -1;
    
    // Loop through each element of the array
    for(i = 0; i < db_data->row_count; i++)
    {
        // is this the one we are after?
        if (db_data->rows[i].id == row_id)
        {
            return i; // return the index found.
        }
    }
    
    // Nothing found...
    return -1;
}

// Delete a row from the data store
void delete_a_row(data_store *db_data)
{
    int row_id, i, row_index;
    
    printf("Please enter id of row to delete: ");
    scanf("%d", &row_id);
    
    // Get the index of the row (will test if db_data == NULL)
    row_index = idx_of_row_with_id(db_data, row_id);
    
    if (row_index >= 0) // a row was found to delete...
    {
        // copy all data past the row, back over the row to delete it
        for(i = row_index; i < db_data->row_count - 1; i++)
        {
            // copy data back one spot in the array (one element at a time)
            db_data->rows[i] = db_data->rows[i+1];
        }
        
        // change the row count, and resize the array
        db_data->row_count--;
        db_data->rows = realloc(db_data->rows, sizeof(row) * db_data->row_count);
    }
}

// Print all of the rows from the data store
void print_all_rows(const data_store *db_data)
{
    int i = 0;
    
    if (db_data == NULL) return;
    
    // For each row in the array
    for (i = 0; i < db_data->row_count; i++)
    {
        // Print the row to the Terminal
        print_row(db_data->rows[i]);
    }
}


// ========
// = Main =
// ========

// Entry point
int main()
{
    menu_option opt;
    data_store db_data = {0, 0, NULL}; 
    
    do
    {
        opt = get_menu_option();
        
        switch(opt)
        {
            case ADD_DATA:
                add_a_row(&db_data);
                break;
            case DELETE_DATA:
                delete_a_row(&db_data);
                break;
            case PRINT_DATA:
                print_all_rows(&db_data);
                break;
            case QUIT:
                printf("Bye.\n");
                break;
        }
    } while(opt != QUIT);
}