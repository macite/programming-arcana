// =======================
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
            break;      //TODO: Add code to draw selected Ellipse and Triangle
        case UNKNOWN:
            break;
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
            break;      //TODO: Add code to draw Ellipse and Triangle shape
        case UNKNOWN:
            break;
    }
}

// Draw the drawing
void draw_drawing(drawing &d)
{
    clear_screen();
    draw_shapes_menu(d.selected_shape);
    
    // Draw all shapes
    for(int i = 0; i< MAX_SHAPES; ++i)
    {
        draw_shape(d.shapes[i]);
    }
}