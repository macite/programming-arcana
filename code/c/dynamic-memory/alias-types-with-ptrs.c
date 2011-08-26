/*program: alias-types-with-ptrs.c */

// The int_ptr type is a pointer to an integer
typedef int *int_ptr;

// The five ints type is an array of five integers
typedef int five_ints[5];

// The ptr_to_array_of_5_ints is a pointer to an array of five ints
typedef int (*ptr_to_array_of_5_ints)[5];

// The five_int_ptrs is an array of five int pointers
typedef int *five_int_ptrs[5];

int main()
{
    int i;
    int x = 10;
    
    // x_ptr is an pointer to x
    int_ptr x_ptr = &x; 
    
    // an array of five ints
    five_ints data = {0,1,2,3,4}; 
    
    // a pointer to the array of 5 ints
    ptr_to_array_of_5_ints data_ptr = &data; 
    
    // an array of five int ptrs
    five_int_ptrs data2 = {&x, &data[0], &data[1], &data[2], &data[3]}; 
    
    printf("%d", x);
    
    for(i = 0; i < 5; i++)
    {
        printf("data[%d] = %d\n", i, data[i]);
        printf("data_ptr[%d] = %p -> %d\n", i, data_ptr[i], (*data_ptr)[i]);
        printf("data2[%d] = %p -> %d\n", i, data_ptr[i], *data2[i]);
        printf("============================\n");
    }
    
    return 0;
}

