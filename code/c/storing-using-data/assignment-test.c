#include "splashkit.h"

/* Program assignment_tests.c
* Demonstrates assignment to a variable. 
*/
int main()
{
    int my_data = 10, days_in_month, days_remaining;
    write("my_data is ");
    write_line(my_data);
    
    my_data = my_data + 1;    //add 1 to my_data and store in my_data
    write("my_data is ");
    write_line(my_data);
    
    my_data++;    //add 1 to my_data and store in my_data
    write("my_data is ");
    write_line(my_data);
    
    my_data *= 2;    //double my_data and store in my_data
    printf("my_data is %d\n", my_data);
    
    days_in_month = 365 / 12; //assign days_in_month a calculated value 
    printf("On average there are " + to_string(days_in_month) + " days in a month.");  
    
    //assign days_remaining a calculated value
    days_remaining = 365 - days_in_month * 12;
    write_line("The remaining " + to_string(days_remaining) + " days are distributed to a few months.",
        days_remaining);
    
    return 0;
}