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

// The Column Value union. Stores either an integer, a
// double or 8 (7 + 1) characters.
typedef union {
        int     int_val;
        //todo: Add double as an option
        char    txt_val[8];
    } column_value;

// The Data Kind enumeration indicates the kind of data
// stored in a row, matches the options available in the
// Column Value union.
typedef enum {
        INT_VAL,
        //todo: Add double as an option
        TXT_VAL,
        UNK_VAL // an unknown value
    } data_kind;

// The Row record/structure. Each row contains an id
// a kind, and some data (a Column Value).
typedef struct {
        int         id;
        data_kind   kind;
        column_value   data;
    } row;

// The data store is a dynamic array of rows, keeping track
// of the number of rows in the array, and the id for the next row
typedef struct {
    int next_row_id;    // The id of the row that will be added next
    int row_count;      // The number of rows in the array
    row *rows;          // A pointer to the rows in memory
} data_store;

// The user can choose to add, delete, or print data or to quit
typedef enum {
    ADD_DATA,
    DELETE_DATA,
    PRINT_DATA,
    QUIT
} menu_option;

// ====================================
// = General Functions and Procedures =
// ====================================

// Trim spaces from the start/end of a string (in place)
// This is passed the string to trim, and the number of characters it contains
void trim(char* text, int n)
{
    int i, j;
    int first_non_space = 0;
    
    // Get the position of the last character
    int last_char = strlen(text);
    if (last_char > n) last_char = n;
    
    // Move back one character - past the null terminator
    if (text[last_char] == '\0') last_char--;
    
    // for each character, back from the last char to the first
    for(i = last_char; i >= 0; i--)
    {
        if (text[i] == ' ') text[i] = '\0'; //replace spaces with null
        else break; // found a non-space so break out of this loop
    }
    
    // remember the new position of the last character
    last_char = i;
    
    // Search forward from the start...
    for(first_non_space = 0; first_non_space < last_char; first_non_space++)
    {
        // Break at the first character that is not a space
        if (text[first_non_space] != ' ') break;
    }
    
    if (first_non_space > 0)
    {
        // Need to copy characters back to start of text...
        // j will track from the start of the text
        j = 0; 
        
        // i will track the index of the non-white space characters
        // starting at the first_non_white space and looping
        // until it gets to the last char (include last char so <= not <)
        for(i = first_non_space; i <= last_char; i++)
        {
            text[j] = text[i];
            j++;
        }
        text[j] = '\0'; // add a null terminator to the end
    }
}

// Test if the passed in text refers to an integer
bool is_integer(const char* text)
{
    char * p;
    long val;
    
    // If the text is empty there is no integer
    if (text == NULL || *text == '\0')
      return false;
    
    // Test that it can be converted to an integer
    val = strtol (text, &p, 10); // base 10
    
    // It is an integer if all characters were used in 
    // the conversion, and there was no range error
    // and the result is in the 
    return *p == '\0' && errno != ERANGE && val <= INT_MAX && val >= INT_MIN;    
}

// Test if the passed in text refers to a double
bool is_double(const char* text)
{
    char * p;
    
    // IF the text is empty there is no double
    if (text == NULL || *text == '\0')
      return false;
    
    // Test that it converts to a double
    strtod (text, &p);
    
    // It is a double if the next character in the text 
    // after the conversion is the end of the string
    return *p == '\0';
}

void clear_input()
{
    scanf("%*[^\n]"); // skip anything is not not a newline
    scanf("%*1[\n]"); // read the newline
}


// =====================================
// = Small DB Functions and Procedures =
// =====================================

// Read a row in from the user and return it. The next_id
// is the id number for the newly created row.
row read_row(int next_id)
{
    char line[16] = "", temp[2];
    row result = {0, UNK_VAL, {0}};
    
    //store the id
    result.id = next_id;
    
    // Read the value from the user into the line
    printf("Enter value: ");
    
    // Read at most 15 characters up to a new line
    // check if only one of the two inputs is matched
    if (scanf("%15[^\n]%1[\n]", line, temp) != 2) 
    {
        // If the next character was not a newline, read
        // any remaining text and skip it
        clear_input();
    }
    
    // Remove any leading or trailing spaces
    trim(line, 16);
    
    // test int first
    if (is_integer(line))
    {
        // read the integer from the line, and store in row
        sscanf(line, "%d", &result.data.int_val);
        // store the kind in the row
        result.kind = INT_VAL;
    }
    else if (is_double(line)) // test dbl
    {
        // todo: Add handling of double...
    }
    else
    {
        // copy the text into the row (at most 7 + 1 characters)
        strncpy(result.data.txt_val, line, 7); // 7 + 1
        // store the kind in the row
        result.kind = TXT_VAL;
    }
    
    printf("Stored in row with id %d\n", result.id);
    return result;
}

// Print the row to the Terminal
void print_row(row to_print)
{
    // Print the row's id
    printf("Row with id %d: ", to_print.id);
    
    // Branch based on the kind, and output the data
    switch (to_print.kind)
    {
        case INT_VAL:
            printf(" has integer %d\n", to_print.data.int_val);
            break;
        // Add double as an option
        case TXT_VAL:
            printf(" has text '%s'\n", to_print.data.txt_val);
            break;
        default: 
            printf(" has an unknown value\n");
    }
}

menu_option get_menu_option()
{
    int input = 0;
    
    printf("=========================\n");
    printf("| Small DB              |\n");
    printf("=========================\n");
    printf(" 1: Add Data\n");
    printf(" 2: Print Data\n");
    printf(" 3: Delete Data\n");
    printf(" 4: Quit\n");
    printf("=========================\n");
    printf("Choose Option: ");
    
    while(scanf("%d", &input) != 1 || input < 1 || input > 4 )
    {
        clear_input();
        printf("Please enter a value between 1 and 4.\n");
        printf("Choose Option: ");
    }
    // Ensure that input is clear after menu is read.
    clear_input();
    
    switch(input)
    {
        case 1: return ADD_DATA;
        case 2: return PRINT_DATA;
        case 3: return DELETE_DATA;
        case 4: return QUIT;
        default: return QUIT;
    }
}

void add_a_row(data_store *db_data)
{
    int row_id = 0;
    
    if (db_data == NULL) return;
    
    // Allocate the id
    row_id = db_data->next_row_id;
    db_data->next_row_id++;
    
    // Store the data
    db_data->row_count++;
    db_data->rows = (row *) realloc(db_data->rows, sizeof(row) * db_data->row_count);
    db_data->rows[db_data->row_count - 1] = read_row(row_id); // last idx = n - 1
}

int idx_of_row_with_id(data_store *db_data, int row_id)
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

void delete_a_row(data_store *db_data)
{
    int row_id;
    int i;
    int row_index;
    
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
    
    // // For each row in the array
    // for (i = 0; i < DB_SIZE; i++)
    // {
    //     // Read the current row's value from the Terminal
    //     db_data[i] = read_row(i);
    // }
    // 
    // // For each row in the array
    // for (i = 0; i < DB_SIZE; i++)
    // {
    //     // Print the row to the Terminal
    //     print_row(db_data[i]);
    // }
    
    return 0;
}