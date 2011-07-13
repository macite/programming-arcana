/* program: test-alias.c */

// Basic alias - number is an alias for int
typedef int number;

// Array alias - five_numbers is an alias for an array of five integers
typedef int five_numbers[5];

// Basic, and Array, c_string and my_string are alias
typedef char *c_string, my_string[256];

// Declaration of const_c_string, a constant c string
typedef const char *const_c_string;

void test(const_c_string text, number idx, five_numbers data)
{
    printf("%s - %d \n", text, data[idx]);
}

int main()
{
    number var1 = 3;
    five_numbers arr1 = {1, 2, 3, 4, 5};
    five_numbers grid[2] = {{1, 2, 3, 4, 5}, 
                            {6, 7, 8, 9, 10} };
    my_string name = "Fred";
    
    printf("%s\n", name);
    
    test("Hello World", var1, arr1);
    test("Grid 0 - ", var1, grid[0]);
    test("Grid 1 - ", var1, grid[1]);
    
    return 0;
}