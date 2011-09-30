#include <stdio.h>

int main()
{
    char message[12] = "Hello World";
    char in_message[6] = "";
    int val = 127;
    int in_val = 0;
    
    FILE *out;
    FILE *in;
    
    out = fopen("test.txt", "w");
    if ( out == NULL ) return -1;
    fprintf(out, "%s\n%d", message, val); //Write the message, and a new line
    fclose(out);
    
    in = fopen("test.txt", "r");
    if ( in == NULL ) return -1;
    
    fscanf(in, "%5[^\n]", in_message); // read first 5 characters of the message
    fscanf(in, "%*[^\n]"); // skip anything else on the line
    fscanf(in, "%*[\n]"); // skip the new line
    fscanf(in, "%d", &in_val); // read the number
    
    fclose(in);
    
    printf("Read: %s & %d\n", in_message, in_val);
    
    return 0;
}