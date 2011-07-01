/*
* Program: test-scanf.c
* Tests some uses for scanf
*/

int main()
{
    int age;
    long int count;
    float val;
    double height;
    long double range;
    
    printf("Please enter age, count, val, height, then range.\nInput: ");
    scanf("%d %ld %f %lf %Lf", &age, &count, &val, &height, &range);
    
    printf("You entered: %d %ld %f %f %Lf\n", age, count, val, height, range);
    
    return 0;
}