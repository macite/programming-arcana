/* Program: test-string.c */
#include <stdio.h>
#include <strings.h>

int main()
{
    const char *sample_text = "Hello World";
    char name[256] = ""; // initialise all 256 characters to null (255 + 1)
    char word[10] = ""; // space for 9 + sentinel
    
    printf("%s has %ld characters\n", sample_text, strlen(sample_text));
    
    printf("Enter a word (upto 9 characters long): ");
    scanf("%9s", word); // no & as this is an array, c will pass it by reference
    printf("You entered %s\n", word);
    
    printf("Enter your full name: ");
    scanf(" %255[^\n]", name); // again no & as name is an array.
    printf("Welcome %s\n", name);
    
    if (strncmp(name, "Fred Smith", 11) == 0)
        printf("Wow, you have the same name as used in the text!\n");
    
    return 0;
}