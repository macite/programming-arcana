#include "splashkit.h"

// This defines that the identifier PI should always be replaced
// with the "value" 3.1415
// This is like "replace all" PI with 3.1415 (Notice no =)
#define PI 3.1415

// This defines a global constant that store the value 
// 3.1415 / 180 ... which is 0.01745277778
// Notice the = ... this means "assign to"
// In this case there is a DEG_TO_RAD "constant"
const float DEG_TO_RAD = PI / 180;

int main()
{
    write_line(PI);
    write_line(DEG_TO_RAD);
    
    return 0;
}