/* Program: parameter-test.c */

void print_equation(int m, double x, int c)
{
    write_line(to_string(m) + " x " + to_string(x) " + " + to_string(c) + " = " + to_string(m * x + c));
}

int main()
{
    print_equation(2, 5.1, 3);
    print_equation(7, 2.74, -8);
    return 0;
}