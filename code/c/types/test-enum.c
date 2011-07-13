/* Program: test-enum.c */

typedef enum
{
    SAFE,
    DANGER,
    EXTREME_DANGER
} warning_level;

int main()
{
    warning_level situation = SAFE;
    
    switch (situation)
    {
        case SAFE: printf("Safe\n"); break;
        case DANGER: printf("Danger!\n"); break;
        case EXTREME_DANGER: printf("Run!\n"); break;
        default: printf("Unknown...\n");
    }
    return 0;
}