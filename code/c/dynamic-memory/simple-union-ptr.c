/* program: union pointer test */

typedef union 
{
    int     *int_ptr;       // stores either an int pointer
    float   *float_ptr;     // or a float pointer
    double  *dbl_ptr;       // or a double pointer
} value_ptr;

int main()
{
    value_ptr ptr, ptr1, ptr2;
    int i = 1.0;
    float f = 1.1;
    double d = 2.2;
    
    ptr.int_ptr = &i;       // Stores a pointer to an int
    ptr1.float_ptr = &f;    // Stores a pointer to a float
    ptr2.dbl_ptr = &d;      // Stores a pointer to a double
    
    return 0;
}