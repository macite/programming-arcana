/*
* Program: lotto-returns.c
* Calculate the return on a Lotto Ticket
*/

#include <math.h>

int combination(int n, int k)
{
    return factl(n) / (factl(k) * (factl(n - k)));
}