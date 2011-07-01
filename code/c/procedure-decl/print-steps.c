/*
* Program: print_steps.c
* Prints out the steps to cook a meal... (partial)
*/

void find_what_to_cook()
{
    printf("Step 1 - Find what to cook -\n");
    printf("1 - Find a recipe book\n");
    printf("2 - Pick recipe \n");
}

void purchase_missing_ingredients()
{
  printf("Step 2(a) - Purchase Missing Ingredients -\n");
  printf("1 - Goto Shop\n");
  printf("2 - Find ingredients and put in basket\n");
  printf("3 - Go to register and pay for ingredients in basket\n");
  printf("4 - Return home\n");
}

void get_ingredients()
{
  printf("Step 2 - Purchase Ingredients -\n");
  printf("1 - Read recipe\n");
  printf("2 - Write a list of ingredients \n");
  printf("3 - Check food stocks, and tick off ingredients you already have\n");
  purchase_missing_ingredients();
}

int main()
{
    find_what_to_cook();
    get_ingredients();
    // etc...
    
    return 0;
}