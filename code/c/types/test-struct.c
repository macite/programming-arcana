/* Program: test-struct.c */
#include <stdio.h>

typedef struct person_struct {
        char    name[50];
        int     age;
    } person;

void print_person(person p)
{
    printf("%s (aged %d)\n", p.name, p.age);
}

void clear_input()
{
    scanf("%*[^\n]"); 
    scanf("%*1[\n]");
}

person get_person(const char *prompt)
{
    person result = { "", 0 };
    
    printf("%s\n", prompt);
    
    printf("Enter name: ");
    scanf("%49[^\n]", result.name);
    
    printf("Enter age: ");
    scanf(" %d", &result.age);
    clear_input();
    
    return result;
}

int main()
{
    person me = {"Fred Smith", 20};
    person friends[2];
    
    friends[0] = get_person("Enter details for a friend.");
    friends[1] = get_person("Enter details for another friend.");
    
    print_person(me);
    print_person(friends[0]);
    print_person(friends[1]);
    
    return 0;
}