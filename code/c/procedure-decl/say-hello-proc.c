/*
* Program: say-hello-proc.c
* Outputs 'Hello' messages to the Terminal.
*/

#include <stdio.h>

void say_hello()
{
    printf("Hello...\n");
}

void say_is_anyone_there()
{
    printf("Is anyone there?");
}

int main()
{
    say_hello();
    say_hello();
    say_is_anyone_there();

    return 0;
}