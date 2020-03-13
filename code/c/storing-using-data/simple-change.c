/*
* Program: simple-change.c
* Calculate the ideal change for a given transaction.
*/
#include "splashkit.h"

#define TWO_DOLLARS 200
#define ONE_DOLLAR 100
#define FIFTY_CENTS 50
#define TWENTY_CENTS 20
#define TEN_CENTS 10
#define FIVE_CENTS 5

int coins_to_give(int change, int coin_value)
{
    return change / coin_value;
}

int give_change(int change_value, int coin_value, string coin_desc)
{
    int to_give;
    
    to_give = coins_to_give(change_value, coin_value);
    write( to_string(to_give) + " x " + coin_desc + ", ");
    
    return change_value - to_give * coin_value;;
}

int get_change_value()
{
    string line;
    int cost_of_item;
    int payment;
    
    write("Cost of item (in cents): ");
    line = read_line();
    cost_of_item = convert_to_int(line);
    
    printf("Amount paid (in cents): ");
    line = read_line();
    payment = convert_to_int(line);
    
    return payment - cost_of_item;
}

int main()
{
    int change_value;
    change_value = get_change_value();
    
    write("Change: ");
    change_value = give_change(change_value, TWO_DOLLARS,  "$2");
    change_value = give_change(change_value, ONE_DOLLAR,   "$1");
    change_value = give_change(change_value, FIFTY_CENTS,  "50c");
    change_value = give_change(change_value, TWENTY_CENTS, "20c");
    change_value = give_change(change_value, TEN_CENTS,    "10c");
    change_value = give_change(change_value, FIVE_CENTS,   "5c");
    write_line();
    
    return 0;
}