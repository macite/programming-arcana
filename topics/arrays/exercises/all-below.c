bool ????(const int data[], int sz, int max)
{
    int i;
    bool result = true;

    for (i = 0; i < sz; i++)
    {
        if (data[i] > max)
        {
          result = false;
          break;
        }
    }
    
    return result;
}
