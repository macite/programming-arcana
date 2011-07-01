/* Program: parameter-test.c */

void print_equation(int m, double x, int c)
{
    printf("%d x %4.2f + %d = %4.2f\n", m, x, c, m * x + c);
}

int main()
{
    print_equation(2, 5.1, 3);
    print_equation(7, 2.74, -8);
    return 0;
}