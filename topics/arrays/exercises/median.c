int median (const int data[], int sz)
{
    int start_idx, end_idx;
    
    start_idx = 0;
    end_idx = sz - 1; // index of last element
    
    while (start_idx != end_idx && start_idx < end_idx)
    {
        start_idx++;
        end_idx--;
    }
    
    if (start_idx == end_idx) 
        return data[start_idx];
    else 
        return data[start_idx] + data[end_idx] / 2;
}