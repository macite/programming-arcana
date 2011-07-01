/* Program: input-test.c */

int main()
{
    int age = 0;
    double num = 0.0;
    
    printf("Please enter your age: ");
    // pass age by reference, allowing scanf to store the value read 
    // into this variable
    scanf("%d", &age);
    
    printf("What is your favourite number: ");
    // pass num by reference
    scanf("%lf", &num);
    
    printf("Thanks, your age is %d\n", age);
    printf("Your favourite number if %f\n", num);
    
    return 0;
}