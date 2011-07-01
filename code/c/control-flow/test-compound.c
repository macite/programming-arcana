/* Program: test-compound.c */

int main()
{
    char ch;
    
    printf("Enter the forest? [y/N]: ");
    scanf("%c", &ch);
    
    if (ch == 'y' || ch == 'Y')
    {
        printf("We are the Knights who say 'Ni'!\n"); 
        printf("We are the keepers of the sacred words: ");
        printf("'Ni', 'Peng', and 'Neee-wom'!\n"); 
        printf("The Knights Who Say 'Ni' demand a sacrifice.\n"); 
        printf("We want... a shrubbery!\n");
    }
    else
    {
        printf("Greetings Sir Robin.\n");
    }
}

