void populate_array(double data[], int size)
{
    int i;
    char prompt[17] = ""; // enough space for "Enter value 99: " + terminator
    char buffer[3] = ""; // enough space for "99" + terminator
    
    for(i = 0; i < size; i++)
    {
        // Ensure that the terminator is included in the copy
        // so that the later calls to strncat know where to
        // append their details. 
        strncpy(prompt, "Enter value ", 13); // 12 + terminator
        sprintf(buffer, "%d", (i + 1) % 100); // % 100 ensure only 2 chars(+ null)
        strncat(prompt, buffer, 2); // takes 2 spaces, terminator moves
        strncat(prompt, ": ", 2); // takes 2 spaces, terminator moves
        
        data[i] = read_double(prompt);
    }
}
