// program: write_binary.c

#include <stdio.h>

int main()
{
    FILE *out;
    
    int count = 3;
    double data[3] = { 73.98, 43.21, 3.1415 };
    
    out = fopen("data.bin", "wb");
    if (out == NULL) return 0;
    
    fwrite(&count, sizeof(int), 1, out); // write number of doubles
    fwrite(data, sizeof(double), 3, out); // write 3 doubles
    
    fclose(out);
    
    return 0;
}