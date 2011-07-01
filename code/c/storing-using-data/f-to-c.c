/*Program: f-to-c.c */
#include <stdio.h>

double fahrenheit_to_celsius(double fahrenheit)
{
    return (5.0 / 9.0) * (fahrenheit-32);
}

int main()
{
    double input_temp, output_temp;
    
    printf("Please enter temperature in Fahrenheit: ");
    scanf("%lf", &input_temp);
    output_temp = fahrenheit_to_celsius(tempF);
    printf("This is %4.2f in Celsius\n", output_temp);
    return 0;
}
