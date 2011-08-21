/* Program: Shape Drawer - SwinGame source file. */
#include <stdio.h>
#include <stdbool.h>
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
    RECTANGLE,
    //TODO: Add Ellipse and Triangle
    UNKNOWN
} shape_type;

// The Shape Data is either...
typedef union
{
    rectangle   rect;
    circle      circ;
    //TODO: Add Ellipse and Triangle
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


// ====================================
// = Declare Functions and Procedures =
// ====================================
// = Drawing Procedures =
// ======================

// Draw the shapes in the menu
void draw_shapes_menu(shape_type selected_shape)
{
    // Draw the "toolbar" area
    fill_rectangle(ColorLightGrey,0,0,60,600);
    
    // Draw the menu shapes.
    fill_circle(ColorGreen, MENU_CIRCLE);
    fill_rectangle(ColorGreen, MENU_RECT);
    //TODO: Add call to draw Ellipse and Triangle
    
    // Redraw the selected shape
    switch(selected_shape)
    {
        case RECTANGLE: 
            draw_rectangle(ColorBlack, MENU_RECT);
            break;
        case CIRCLE:
            draw_circle(ColorBlack, MENU_CIRCLE);
            break;
        //TODO: Add code to draw selected Ellipse and Triangle
        case UNKNOWN:
            break;
        // Do nothing for default...
    }
}

// Draw a shape onto the screen
void draw_shape(shape &s)
{
    switch(s.type)
    {
        case RECTANGLE:
            fill_rectangle(s.fill_color, s.data.rect);
            break;
        case CIRCLE:
            fill_circle(s.fill_color,s.data.circ);
            break;
        //TODO: Add code to draw Ellipse and Triangle shape
        case UNKNOWN:
            break;
    }
}

// Draw the drawing
void draw_drawing(drawing &d)
{   
    // Clear screen and redraw menu
    clear_screen();
    draw_shapes_menu(d.selected_shape);
    
    // Draw the shapes
    for(int i = 0; i< MAX_SHAPES; ++i)
    {
        draw_shape(d.shapes[i]);
    }
}

// ==========================================
// = Procedures to interact with Drawing... =
// ==========================================

// Clear the drawing
void clear_drawing(drawing &d)
{
    // Start adding at index 0
    d.index = 0;
    
    // All shapes are unknown...
    for(int i = 0; i < MAX_SHAPES ;++i) 
    {
        d.shapes[i].type = UNKNOWN;
    }
}

// Add a rectangle to the drawing
void make_rectangle(shape &s, point2d pt)
{
    // Set the shape
    s.type = RECTANGLE;
    s.fill_color = ColorRed;
    
    // Copy in the menu rectangle
    s.data.rect = MENU_RECT;
    
    // Change its position
    s.data.rect.x = pt.x;
    s.data.rect.y = pt.y;
}

// Add a circle to the drawing
void make_circle(shape &s, point2d pt)
{
    // Set the shape
    s.type = CIRCLE;
    s.fill_color = ColorBlue;
    
    // Copy in the menu circle radius
    s.data.circ.radius = MENU_CIRCLE.radius;
    
    // Set the position
    s.data.circ.center = pt;
}

//TODO: Add procedures to add Ellipse and Triangle shapes

// ==========================================
// = Procedures to handle user interactions =
// ==========================================

// C clears, q quits
bool process_key(drawing &d)
{
    if(key_down(VK_Q)) 
        return true;
    
    if(key_down(VK_C))
        clear_drawing(d);
    
    return false;   
}

// Check if user clicked in a shape in the toolbar
void process_menu_click(drawing &d, point2d pt)
{   
    if(point_in_rect(&pt,&MENU_RECT))
    {
        d.selected_shape = RECTANGLE;
    }
    else if(point_in_circle(&pt, &MENU_CIRCLE))
    {
        d.selected_shape = CIRCLE;
    }
    //TODO: Add code to test if user clicked in the Ellipse or Triangle
}

// Add a shape to the drawing canvas
void process_canvas_click(drawing &d, point2d pt)
{
    // Try to add a shape... is the current index < maximum?
    if(d.index < MAX_SHAPES)
    {
        // Select the shape to add...
        switch(d.selected_shape)
        {
            case RECTANGLE:
                make_rectangle(d.shapes[d.index], pt);
                break;
            case CIRCLE:
                make_circle(d.shapes[d.index], pt);
                break;
            //TODO: Add code to call make the shape an Ellipse / Triangle
            default:
                return; // exit as no selected shape... (doesn't increment index)
        }
        
        // Increment the index
        d.index++;
    }
}

// Check if the user has performed any actions...
bool process_input(drawing &d)
{
    if(mouse_clicked(LEFT_BUTTON))
    {
        point2d pt = mouse_position();
        
        if(pt.x < MENU_RIGHT_X)
            process_menu_click(d, pt);
        else            
            process_canvas_click(d, pt);
    }
    
    if(any_key_pressed())  
        return process_key(d);
    else
        return false;
}

//main  
int main()
{
    // Create the drawing...
    drawing my_drawing;
    bool quit = false;
    
    // Initialise the drawing with empty data...
    clear_drawing(my_drawing);
            
    open_graphics_window("Draw Shapes", 800, 500);
    load_default_colors();
    
    do
    {
        process_events(); // read user interactions...
        quit = process_input(my_drawing);
        
        draw_drawing(my_drawing);
        refresh_screen();
    } while ( ! window_close_requested() && !quit);
     
    release_all_resources();
    return 0;
}




