// Draw a shape onto the screen
procedure DrawShape(var s: Shape);
begin
    case s.kind of
        RECTANGLE_TYPE: FillRectangle(s.FillColor, s.rect);
        CIRCLE_TYPE:    FillCircle(s.FillColor, s.circ);
        //Todo: Add Ellipse and Triangle
    end; //end case
end;

procedure DoDrawing(var d: Drawing); // Draw the Drawing
var
    i: integer;
begin   
    // Clear screen and redraw menu
    ClearScreen(ColorWhite);
    DrawShapesMenu(d.SelectedShape);
    
    // Draw the shapes
    for i := 0 to MAX_SHAPES do
        DrawShape(d.Shapes[i]);
end; //do Drawing

// ============================
// = Procedures to Add Shapes =
// ============================

procedure AddRectangle(var s: Shape; pt: Point2d);
begin
    // Set the shape
    s.kind := RECTANGLE_TYPE;
    s.FillColor := ColorRed;
    
    // Copy in the menu Rectangle
    s.rect := MENU_RECT;
    
    // Change its position
    s.rect.x := pt.x;
    s.rect.y := pt.y;
end;

procedure AddCircle(var s: shape; pt: Point2d);
begin
    // Set the shape
    s.kind := CIRCLE_TYPE;
    s.FillColor := ColorBlue;
    
    // Copy in the menu Circle radius
    s.circ.Radius := MENU_CIRCLE.Radius;
    
    // Set the position
    s.circ.Center := pt;
end;//add Circle

//Todo: Add AddEllipse and AddTriangle