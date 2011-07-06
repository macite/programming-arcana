/* Program: test-for.c */
#include <stdio.h>

void print_characters(const char *text, int size)
{
    int i;
    for (i = 0; i < size; i++)
    {
        printf("%c (ASCII %hd) at index %d\n", text[i], text[i], i);
    }
}

int main()
{
    print_characters("Hello World", 12);
    print_characters("Fred", 5);
    return 0;
}