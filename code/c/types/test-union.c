/* program: test-union.c */

typedef unsigned char byte;

typedef union color_union
{
    unsigned int value;
    byte components[4];
} color;

int main()
{
    color red;
    red.components[0] = 255; // r
    red.components[1] = 0;   // g
    red.components[2] = 0;   // b
    red.components[3] = 255; // a
    
    // Warning: Unsafe use, assumes knowledge of underlying layout
    // of the integer that will not work on all CPUs.
    printf("%x\n", red.value);
    return 0;
}