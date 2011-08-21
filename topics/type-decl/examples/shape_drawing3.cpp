// ==========================================
// = Procedures to handle user interactions =
// ==========================================

// Check key presses - c clears, q quits
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