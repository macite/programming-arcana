/* Program: test-file.c */

#include <stdio.h>
#include <stdbool.h>

bool populate_from_file(int data[], int sz, char *filename)
{
    int i;
    FILE *input = fopen(filename, "r");
    
    if (input == NULL) return false;
    
    for (i = 0; i < sz; i++)
    {
        if (fscanf(input, " %d", &data[i]) != 1)
        {
            fclose(input);
            return false;
        }
    }
    
    fclose(input);
    return true;
}

void print_data(int data[], int sz)
{
    int i;
    
    for(i = 0; i < sz; i++)
    {
        printf("data[%d]: %d\n", i, data[i]);
    }
}


int main()
{
    int data[3];
    
    if (populate_from_file(data, 3, "data.txt"))
    {
        print_data(data, 3);
    }
    else
    {
        printf("Error in data.txt.\n");
    }
    
    return 0;
}