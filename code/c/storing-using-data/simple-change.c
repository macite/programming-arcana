/*
* Program: simple-change.c
* Calculate the ideal change for a given transaction.
*/
#include <stdio.h>

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

void output_change_data(int number_given, char *coin_desc)
{
    printf("%d x %s, ", number_given, coin_desc);
}

int main()
{
    int cost_of_item;
    int payment;
    int change_value;
    int to_give;
    
    printf("Cost of item (in cents): ");
    scanf("%d", &cost_of_item);
    
    printf("Amount paid (in cents): ");
    scanf("%d", &payment);
    
    printf("Change: ");
    change_value = payment - cost_of_item;
    
    to_give = coins_to_give(change_value, TWO_DOLLARS);
    change_value = change_value - to_give * TWO_DOLLARS;
    output_change_data(to_give, "$2");
    
    to_give = coins_to_give(change_value, ONE_DOLLAR);
    change_value = change_value - to_give * ONE_DOLLAR;
    output_change_data(to_give, "$1");
    
    to_give = coins_to_give(change_value, FIFTY_CENTS);
    change_value = change_value - to_give * FIFTY_CENTS;
    output_change_data(to_give, "50c");
    
    to_give = coins_to_give(change_value, TWENTY_CENTS);
    change_value = change_value - to_give * TWENTY_CENTS;
    output_change_data(to_give, "20c");
    
    to_give = coins_to_give(change_value, TEN_CENTS);
    change_value = change_value - to_give * TEN_CENTS;
    output_change_data(to_give, "10c");
    
    to_give = coins_to_give(change_value, FIVE_CENTS);
    change_value = change_value - to_give * FIVE_CENTS;
    output_change_data(to_give, "5c");
    return 0;
}