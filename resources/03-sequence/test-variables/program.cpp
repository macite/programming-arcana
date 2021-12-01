#include "splashkit.h"

int main()
{
    int age;
    string name;
    string line;

    write("What is your name: ");
    name = read_line();

    write("What is your age: ");
    line = read_line();
    age = convert_to_integer(line);

    write_line("Hello " + name);
    write("Next year you will be ");
    write(age + 1);
    write_line(" years old");

    return 0;
}
