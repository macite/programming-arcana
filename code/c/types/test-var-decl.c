/* Program: test-var-decl.c */
#include <stdio.h>
#include <strings.h>

typedef struct
{
    int     field1;
    double  field2;
    char    field3[10];
} my_struct;

typedef enum 
{
    OPT_1,
    OPT_2
} my_enum;

typedef union
{
    long long   long_val;
    double      double_val;
} my_number;

int main()
{
    // Declare variables, strucutres, enums, and unions.
    my_struct var1;
    my_struct var2 = {1, 3.1415, "Fred"};
    my_struct var3 = var2;
    
    my_enum var4 = OPT_1;
    
    my_number var5;
    
    // Play with record/structures 
    var1.field1 = 2;
    var1.field2 = 6.5;
    strncpy(var1.field3, "Wilma", 9);
    
    printf("%s %d %f %s\n", "var1", var1.field1, var1.field2, var1.field3);
    printf("%s %d %f %s\n", "var2", var2.field1, var2.field2, var2.field3);
    printf("%s %d %f %s\n", "var3", var3.field1, var3.field2, var3.field3);
    
    // Play with enums
    printf("Int value of var4 is %d\n", var4);
    var4 = OPT_2;
    printf("Int value of var4 is %d\n", var4);
    
    // Play with unions
    var5.long_val = 123456;
    printf("%lld\n", var5.long_val);
    
    var5.double_val = 3.1415;
    printf("%f\n", var5.double_val); //note: long_val has been written over...
    
    return 0;
}