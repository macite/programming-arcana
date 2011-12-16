void ????(int data[], int sz, int param3, int param4)
{
    int i;
    
    for (i = sz - 1; i > param3; i--)
    {
        data[i] = data[i - 1];
    }
    
    data[param3] = param4;
}