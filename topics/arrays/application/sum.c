// Calculate the sum of the values in the array
double sum(double data[], int size)
{
    int i;
    double total = 0;
    
    for(i = 0; i < size; i++)
    {
        total += data[i];
    }
    
    return total;
}
