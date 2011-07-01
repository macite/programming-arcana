bool perform_guess(int num_guess, int target)
{
    int guess;
    
    printf("Guess %d: ", num_guess);
    scanf("%d", &guess);
    
    if (target < guess) 
    {
        printf("The number is less than %d\n", guess);
    }
    else
    {
        if (target > guess) 
            printf("The number is larger than %d\n", guess);
        else 
            printf("Well done... the number was %d\n", guess);
    }
    
    return target == guess;
}