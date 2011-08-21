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

//TODO: Add procedures to make Ellipse and Triangle shapes



