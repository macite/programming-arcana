/* Program: small-db.c */
#include <stdio.h>
#include <strings.h>
#include <stdbool.h>
#include <stdlib.h>
#include <errno.h>

// The number of elements in the row array
#define DB_SIZE 3

// The Column Value union. Stores either an integer, a
// double or 8 (7 + 1) characters.
typedef union {
        int     int_val;
        double  dbl_val;
        char    txt_val[8];
    } column_value;

// The Data Kind enumeration indicates the kind of data
// stored in a row, matches the options available in the
// Column Value union.
typedef enum {
        INT_VAL,
        DBL_VAL,
        TXT_VAL
    } data_kind;

// The Row record/structure. Each row contains an id
// a kind, and some data (a Column Value).
typedef struct {
        int         id;
        data_kind   kind;
        column_value   data;
    } row;

// Test if the passed in text refers to an integer
bool is_integer(const char* text)
{
    char * p;
    long int val;
    
    // If the text is empty there is no integer
    if (text == NULL || *text == '\0')
      return false;
    
    // Test that it can be converted to an integer
    val = strtol (text, &p, 10); // base 10
    
    // It is an integer if all characters were used in 
    // the conversion, and there was no range error
    return *p == '\0' && errno != ERANGE;    
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

// Display the intro message.
void show_intro()
{
    printf("%s%s%s%s%d%s%s%s",
        "-----------------------\n",
        "  Welcome to Small DB\n",
        "-----------------------\n",
        "Please enter ", DB_SIZE, " values.\n",
        "They can be text\n",
        "or numbers.\n");
}

// Read a row in from the user and return it. The next_id
// is the id number for the newly created row.
row read_row(int next_id)
{
    char line[16] = "", temp[2];
    row result = {0, INT_VAL, {0}};
    
    //store the id
    result.id = next_id;
    
    // Read the value from the user into the line
    printf("Enter value: ");
    
    // Read at most 15 characrters up to a new line
    // check if only one of the two inputs is matched
    if (scanf(" %15[^\n]%1[\n]", line, temp) == 1) 
    {
        // If the next character was not a newline, read
        // any remaining text and skip it
        scanf("%*[^\n]");
    }
    
    // test int first
    if (is_integer(line))
    {
        // was an integer
        // read the integer from the line, and store in row
        sscanf(line, "%d", &result.data.int_val);
        // store the kind in the row
        result.kind = INT_VAL;
    }
    else if (is_double(line)) // test dbl
    {
        // was a double
        // read the double from the line, and store in row
        sscanf(line, "%lf", &result.data.dbl_val);
        // store the kind in the row
        result.kind = DBL_VAL;
    }
    else
    {
        // is not integer or double, so its text
        
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
        case DBL_VAL: 
            printf(" has double %f\n", to_print.data.dbl_val);
            break;
        case TXT_VAL:
            printf(" has text '%s'\n", to_print.data.txt_val);
            break;
        default: 
            printf(" has an unknown value\n");
    }
}


// Entry point
int main()
{
    // Create array or Column Values
    row db_data[DB_SIZE];
    int i;
    
    show_intro();
    
    // For each row in the array
    for (i = 0; i < DB_SIZE; i++)
    {
        // Read the current row's value from the Terminal
        db_data[i] = read_row(i);
    }
    
    // For each row in the array
    for (i = 0; i < DB_SIZE; i++)
    {
        // Print the row to the Terminal
        print_row(db_data[i]);
    }
    
    return 0;
}