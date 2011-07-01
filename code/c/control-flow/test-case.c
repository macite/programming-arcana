/* Program: test-case.c */

#include<stdio.h>

int read_menu_option()
{
    int result = 0;
    
    printf("1: Greet the Knights of Ni\n");
    printf("2: Offer Knights a shrubbery\n");
    printf("3: Refuse to cut down tree with Herring\n");
    printf("4: Tell them all about it\n");
    
    printf("Option: ");
    scanf("%d", &result);
    return result;
}

int main()
{
    int option = read_menu_option();
    
    switch(option)
    {
        case 1: printf("We say Ni to you!\n"); 
                break;
        case 2: printf("Cut down the mightiest tree... with a Herring!\n"); 
                break;
        case 3: printf("Oh please..."); 
                break;
        case 4: printf("Argh... dont say that word!\n") 
                break;
        default: 
                printf("Please enter a value between 1 and 4");
    }
    return 0;
}