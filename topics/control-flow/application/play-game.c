void play_game()
{
    int my_number, guess_num;
    bool got_it;
    
    my_number = random() % MAX_NUMBER + 1;
    guess_num = 0; //Keep track of the number of guesses
    
    printf("I am thinking of a number between 1 and %d\n\n", MAX_NUMBER);
    
    do
    {
        guess_num++;
        got_it = perform_guess(guess_num, my_number);
    }
    while( guess_num < MAX_GUESSES && !got_it);
    
    if ( !got_it )
    {
        printf("You ran out of guesses... the number was %d\n", my_number);
    }
}
