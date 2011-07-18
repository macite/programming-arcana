void delete_a_row(data_store *db_data)
{
    int row_id;
    row *current, *next, *prev;
    
    if (db_data == NULL) return;
    
    printf("Please enter id of row to delete: ");
    scanf("%d", &row_id);
    
    current = db_data->first_row; // Start searching for the row to delete
    prev = NULL; // There is no previous for the first row
    
    while(current != NULL && current->id != row_id)
    {
        prev = current;             // Old current is new prev
        current = current->next;    // New current is current's next
    }
    
    next = current->next;  // get the new "next" Row
    prev->next = next;     // skip the current Row
    free(current);         // Release the memory
}
