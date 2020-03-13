/* Program: input-test.c */

int main()
{
    int age = 0;
    double num = 0.0;
    string line;
    
    write("Please enter your age: ");
    // pass age by reference, allowing scanf to store the value read 
    // into this variable
    line = read_line();
    age = convert_to_integer(line);
    
    write("What is your favourite number: ");
    // pass num by reference
    line = read_line();
    num = convert_to_double(line);
    
    write_line("Thanks, your age is " + to_string(age));
    write_line("Your favourite number is " + to_string(num));
    
    return 0;
}