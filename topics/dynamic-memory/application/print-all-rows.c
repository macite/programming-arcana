void print_all_rows(const data_store *db_data)
{
    row *current;
    
    if (db_data == NULL) return;
    
    current = db_data->first_row;
    
    // While there is a current node
    while(current != NULL)
    {
        // Print the row to the Terminal
        print_row(*current);        // Follow the pointer, pass the value
        current = current->next;    // Move to next Row
    }
}
