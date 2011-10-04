#include <stdio.h>
#include <stdlib.h>

int main()
{
    FILE *in;
    int i, count;
    double *data;   // dynamic array
    
    in = fopen("data.bin", "rb");
    if (in == NULL) return 0;
    
    fread(&count, sizeof(int), 1, in);  // read the count
    
    data = calloc(count, sizeof(double)); // allocate space for "count" doubles
    if (data == NULL) return 0;
    
    fread(data, sizeof(double), count, in); // read data from the file
    fclose(in);
    
    for ( i = 0 ; i < count; i++ )
    {
        printf("data[%d] = %lf\n", i, data[i]);
    }
    
    free(data); // Free the space allocated to the data "array"
    
    return 0;
}