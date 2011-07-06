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
        strncpy(prompt, "Enter value ", 13); // plus terminator
        sprintf(buffer, "%2d", i + 1);
        strncat(prompt, buffer, 2); 
        strncat(prompt, ": ", 3); // plus terminator
        
        data[i] = read_double(prompt);
    }
}
