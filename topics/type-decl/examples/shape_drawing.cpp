/* Program: Shape Drawer - SwinGame source file. (C++) */
#include "SwinGame.h"

// =====================
// = Declare Constants =
// =====================
#define MAX_SHAPES  20
#define MENU_RIGHT_X 60

// Menu Shape Constants
const rectangle MENU_RECT       = {10,250,40,50};
const circle    MENU_CIRCLE     = {{30,100},20};
const triangle  MENU_TRIANGLE   = {{30,150},{10,200},{50,200}};
const rectangle MENU_ELLIPSE    = {10,350,40,50};
const rectangle DRAWING_PAD     = {61,0,790,600};

// =================
// = Declare Types =
// =================

// The Shape Type is one of these options
typedef enum
{
    CIRCLE,
    RECTANGLE, //TODO: Add Ellipse and Triangle
    UNKNOWN
} shape_type;

// The Shape Data is either...
typedef union
{
    rectangle   rect;
    circle      circ; //TODO: Add Ellipse (as rectangle) and Triangle
} shape_data;

// The shape records...
typedef struct
{
    shape_type  type;        // the option selected
    color       fill_color;  // the fill color
    shape_data  data;        // the shape's data
} shape;

// A Drawing records...
typedef struct
{
    shape       shapes[MAX_SHAPES]; // a number of shapes
    int         index;              // the index of the last shape used
    shape_type  selected_shape;     // the selected shape type
} drawing;
