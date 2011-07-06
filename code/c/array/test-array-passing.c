/* Program: test-array-passing.c */

void test_pass_in_only_v1(const int data[], int size)
{
    printf("Can only read from data -> %d...%d\n", data[0], data[size - 1]);
}

void test_pass_in_and_out_v1(int data[], int size)
{
    printf("Can read and change data\n");
    data[0] = data[0] + 1;                  //increment first
    data[size - 1] = data[size - 1] * 2;    //double last
}

//----------------------------------------------------------------

void test_pass_in_only_v2(const int *data, int size)
{
    printf("Can only read from data -> %d...%d\n", data[0], data[size - 1]);
}

void test_pass_in_and_out_v2(int *data, int size)
{
    printf("Can read and change data\n");
    data[0] = data[0] + 1;                  //increment first
    data[size - 1] = data[size - 1] * 2;    //double last
}

//----------------------------------------------------------------

void say_hello_to(const char *name)
{
    printf("Hello %s!\n", name);
}

//----------------------------------------------------------------


int main()
{
    int my_data[] = {1, 2, 3};
    int other_data[] = {1, 2};
    
    say_hello_to("Fred");
    
    test_pass_in_and_out_v1(my_data, 3);
    test_pass_in_and_out_v2(other_data, 2);
    
    test_pass_in_only_v1(my_data, 3);
    test_pass_in_only_v1(other_data, 2);
    
    return 0;
}