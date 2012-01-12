//input processing procedures
function ProcessKey(var d: Drawing): Boolean;
var
    i: integer;
begin
    if KeyDown(VK_Q) then
        result := true;
        exit;
    if KeyDown(VK_C) then
    begin
        d.Index := 0;
        for i := 0 to MAX_SHAPES do
            d.Shapes[i].kind := UNKNOWN_TYPE;
    end;
    result := false;    
end;

// Check if the user has clicked in a shape in the toolbar
procedure ProcessMenuClick(var d: Drawing; pt: Point2D);
begin   
    if PointInRect(pt, MENU_RECT) then
        d.SelectedShape := RECTANGLE_TYPE;
    
    if PointInCircle(pt, MENU_CIRCLE) then
        d.SelectedShape := CIRCLE_TYPE;
        
    //Todo: Add Ellipse (check in rect) and Triangle
end;

// Add a shape to the canvas
procedure ProcessCanvasClick(var d: Drawing; pt: Point2D);
begin
    // Try to add a shape... is the current index < maximum?
    if(d.Index < MAX_SHAPES) then
    begin
        // Select the shape to add...
        case(d.SelectedShape) of
            RECTANGLE_TYPE: AddRectangle(d.Shapes[d.Index], pt);
            CIRCLE_TYPE:    AddCircle(d.Shapes[d.Index], pt);
            //Todo: Add triangle and ellipse
        else
            exit; // dont add to index
        end;    //end case
        
        d.Index := d.Index + 1;
    end;    //end if
end;    // process pad click