// Previous code for small db remains unchanged...

// ===========================
// = Loading and Saving Data =
// ===========================

void read_row_from_file(row *to_load, FILE *input)
{
    // Load defaults in case loading fails...
    to_load->id = 0;
    to_load->kind = INT_VAL;
    to_load->data.int_val = 0;
    
    // Read in the id and the kind (as an integer)
    fscanf(input, " %d %d ", &to_load->id, (int*)&to_load->kind);
    
    // Branch based on the kind, and output the data
    switch (to_load->kind)
    {
        case INT_VAL:
            // Read in the integer from the file
            fscanf(input, "%d\n", &to_load->data.int_val);
            break;
        // Add double as an option
        case TXT_VAL:
            // Read in the text value from the file (upto 7 characters)
            fscanf(input, "%7[^\n]\n", to_load->data.txt_val); // no & as char array
            break;
        default:
            // Dont know what the value is... set it to 0
            to_load->data.int_val = 0;
    }
}

void load(data_store *db_data, const char *filename)
{
    FILE *input;
    
    input = fopen(filename, "r");
    if ( input == NULL ) return;
    
    int i = 0;
    
    if (db_data != NULL)
    {
        // Store 0 rows as a default, in case load fails
        db_data->row_count = 0;
        
        // Save the row count...
        fscanf(input, "next id:%d ", &(db_data->next_row_id));
        fscanf(input, "rows:%d ", &(db_data->row_count));
        
        // Allocate space for rows...
        db_data->rows = (row *) realloc(db_data->rows, sizeof(row) * db_data->row_count);
        if (db_data->rows == NULL)
        {
            fclose(input);
            return;
        }
        
        // For each row in the array
        for (i = 0; i < db_data->row_count; i++)
        {
            // read the row from the file, into the space allocated for this row
            read_row_from_file(&(db_data->rows[i]), input);
        }        
    }
    
    fclose(input);
    return;
}

void save_row(row to_save, FILE *out)
{
    fprintf(out, "%d %d ", to_save.id, to_save.kind);
    
    // Branch based on the kind, and output the data
    switch (to_save.kind)
    {
        case INT_VAL:
            fprintf(out, "%d\n", to_save.data.int_val);
            break;
        // Add double as an option
        case TXT_VAL:
            fprintf(out, "%s\n", to_save.data.txt_val);
            break;
        default:
            fprintf(out, "\n"); //dont save unknown data
    }
    
}

void save(const data_store *db_data, const char *filename)
{
    FILE *out;
    int i;
    
    out = fopen(filename, "w");
    if ( out == NULL ) return;

    if (db_data != NULL)
    {
        // Save the row count...
        fprintf(out, "next id:%d\n", db_data->next_row_id);
        fprintf(out, "rows:%d\n", db_data->row_count);
        
        // For each row in the array
        for (i = 0; i < db_data->row_count; i++)
        {
            save_row(db_data->rows[i], out);
        }        
    }
    
    fclose(out);
    return;
}

// ========
// = Main =
// ========

// Entry point
int main()
{
    menu_option opt;
    data_store db_data = {0, 0, NULL}; 
    
    load(&db_data, "data.txt");
    
    do
    {
        // Code as before ...
    } while(opt != QUIT);
    
    save(&db_data, "data.txt");
    
    return 0;
}
