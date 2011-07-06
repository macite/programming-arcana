double read_double(const char *prompt)
{
    double result = 0;
    
    printf("%s", prompt);
    while (scanf(" %lf", &result) != 1) // Read value, and try to convert double
    {
        // Convert failed, as input was not a number
        scanf("%*[^\n]"); // Read past the end of the current line
        printf("Please enter a number.\n");
        printf("%s", prompt);
    }
    
    return result;
}
