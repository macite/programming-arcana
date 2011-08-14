bool ????(const int data[], int sz, int val)
{
    int i;
    bool result = false;
    
    for (i = 0; i < sz; i++)
    {
        if (data[i] == val)
        {
            result = true;
            break;
        }
    }
    
    return result;
}