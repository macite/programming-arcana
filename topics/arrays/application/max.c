// Find the largest value in the array
double max(double data[], int size)
{
    int i;
    double max = data[0];
    
    for (i = 1; i < size; i++)
    {
        if (data[i] > max) max = data[i];
    }
    
    return max;
}
