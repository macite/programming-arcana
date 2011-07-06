// ... header missing
double fahrenheit_to_celsius(double fahrenheit)
{
    return (5.0 / 9.0) * (fahrenheit-32);
}

int main()
{
    double input_temp;
    printf("Please enter temperature in Fahrenheit: ");
    scanf("%lf", &input_temp);
    printf("This is %4.2f in Celsius\n", fahrenheit_to_celsius(tempF));
    return 0;
}