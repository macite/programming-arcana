// Program: text_io.h

#include <stdio.h>

int main()
{
    FILE *out, *in;
    
    int to_save = 20, to_load;
    
    out = fopen("text.dat", "w");
    if (out == NULL) return 0;
    fprintf(out, "%d", to_save);
    fclose(out);
    
    in = fopen("text.dat", "r");
    if (in == NULL) return 0;
    fscanf(in, "%d", &to_load);
    fclose(in);
    
    printf("Wrote %d, read %d\n", to_save, to_load);
    return 0;
}