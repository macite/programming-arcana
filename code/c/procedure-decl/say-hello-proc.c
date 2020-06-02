/*
* Program: say-hello-proc.c
* Outputs 'Hello' messages to the Terminal.
*/

#include "splashkit.h"

void say_hello()
{
    write_line("Hello...");
}

void say_is_anyone_there()
{
    write_line("Is anyone there?");
}

int main()
{
    say_hello();
    say_hello();
    say_is_anyone_there();

    return 0;
}