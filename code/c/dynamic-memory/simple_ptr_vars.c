#include <stdio.h>

void read_data(const char *prompt, const char *format_tag, void *p)
{
    printf("%s", prompt);
    scanf(format_tag, p);
}

void print_intp(int *ptr)
{
    printf("%p -> %d\n", ptr, *ptr);
}

int main()
{
    int i, j;
    float x;
    int *p;
    int *ptrs[] = { &i, &j };
    
    read_data("Enter a value for i:", "%d", &i);
    read_data("Enter a value for j:", "%d", &j);
    read_data("Enter a value for x:", "%f", &x);
    
    p = &i;
    print_intp(p);
    print_intp(&i);
    print_intp(&j);
    print_intp(ptrs[1]);
    
    return 0;
}