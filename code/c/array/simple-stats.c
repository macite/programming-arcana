/* stats-calc.c */

#include <stdio.h>
#include <math.h>
#include <string.h>

#define DATA_SIZE 10

// Calculate the sum of the values in the array
double sum(const double data[], int size)
{
    int i;
    double result = 0;
    
    for(i = 0; i < size; i++)
    {
        result += data[i];
    }
    
    return result;
}

// Calculate the mean of the values in the array
double mean(const double data[], int size)
{
    return sum(data, size) / size;
}

// Find the largest value in the array
double max(const double data[], int size)
{
    //todo: add logic here...
    return 0;
}

// Find the standard deviation of the values in the array
double variance(const double data[], int size)
{
    //todo: add logic here...
    return 0;
}

double read_double(const char *prompt)
{
    double result;
    
    printf("%s", prompt);
    while (scanf(" %lf", &result) != 1)
    {
        scanf("%*[^\n]");
        printf("Please enter a number.\n");
        printf("%s", prompt);
    }
    
    return result;
}

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

// Implements a simple guessing game. The program generates
// a random number, and the player tries to guess it.
int main()
{
    double data[DATA_SIZE];
    
    populate_array(data, DATA_SIZE);
    
    printf("\nCalculating statistics...\n\n");
    
    printf("Sum:        %4.2f\n", sum(data, DATA_SIZE));
    printf("Mean:       %4.2f\n", mean(data, DATA_SIZE));
    printf("Variance:   %4.2f\n", variance(data, DATA_SIZE));
    printf("Max:        %4.2f\n", max(data, DATA_SIZE));
    
    return 0;
}
