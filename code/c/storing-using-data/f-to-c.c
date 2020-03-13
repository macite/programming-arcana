// ... header missing

double fahrenheit_to_celsius(double fahrenheit)
{
    return (5.0 / 9.0) * (fahrenheit-32);
}

int main()
{
    double input_temp;
    string line;

    write("Please enter temperature in Fahrenheit: ");
    line = read_line();
    input_temp = convert_to_double(line);

    write_line("This is " + to_string(fahrenheit_to_celsius(input_temp)) + " in Celsius");
    return 0;
}