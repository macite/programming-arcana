program ShapeDrawer;
uses
    sgTypes, sgInput, sgAudio, sgGraphics, sgResources, sgUtils, sgText, sgImages, sgPhysics, sgGeometry;
    
// =====================
// = Declare Constants =
// =====================
const MAX_SHAPES = 20;

const MENU_RECT :       Rectangle = (x: 10; y: 250; width: 40; height: 50);
const MENU_CIRCLE :     Circle = (center: (x: 30; y: 100); radius: 20);
const MENU_TRIANGLE :   Triangle = ((x: 30; y: 150), (x: 10; y: 200), (x: 50; y: 200));
const MENU_ELLIPSE :    Rectangle = (x: 10; y: 350; width: 40; height: 50);
const DRAWING_PAD :     Rectangle = (x: 61; y: 0; width: 790; height: 600);

// =================
// = Declare Types =
// =================

// The Shape Type is one of these options
type
    ShapeType = ( CIRCLE_TYPE, TRIANGLE_TYPE, RECTANGLE_TYPE, ELLIPSE_TYPE, UNKNOWN_TYPE );

    // The shape records...
    Shape = record
        fillColor: Color;
        case kind: ShapeType of // The Shape Data is either...
            CIRCLE_TYPE:    ( circ:     Circle;    );
            TRIANGLE_TYPE:  ( tri:      Triangle;  );
            RECTANGLE_TYPE: ( rect:     Rectangle; );
            ELLIPSE_TYPE:   ( ellipse:  Rectangle; );
    end;
    
    // A Drawing records...
    Drawing = record
        Shapes: array[0..MAX_SHAPES] of Shape;  // a number of shapes
        Index: integer;                         // the index of the last shape used
        SelectedShape: ShapeType;               // the selected shape type
    end;

// ====================================
// = Declare Functions and Procedures =
// ====================================


// ======================
// = Drawing Procedures =
// ======================

// Draw the shapes in the menu
procedure DrawShapesMenu(selectedShape: ShapeType);
begin
    // Draw the "toolbar" area
    FillRectangle(ColorLightGrey,0,0,60,600);
    
    // Draw the menu shapes.
    FillCircle(ColorGreen, MENU_CIRCLE);
    FillTriangle(ColorGreen, MENU_TRIANGLE);
    FillRectangle(ColorGreen, MENU_RECT);
    FillEllipse(ColorGreen, MENU_ELLIPSE);
    
    // Redraw the selected shape
    case(SelectedShape) of
        RECTANGLE_TYPE: DrawRectangle(ColorBlack, MENU_RECT);
        TRIANGLE_TYPE:  DrawTriangle(ColorBlack, MENU_TRIANGLE);
        CIRCLE_TYPE:    DrawCircle(ColorBlack, MENU_CIRCLE);
        ELLIPSE_TYPE:   DrawEllipse(ColorBlack, MENU_ELLIPSE);
        UNKNOWN_TYPE:   exit; // Do nothing for default...
    end;    
end;

// Draw a shape onto the screen
procedure DrawShape(var s: Shape);
begin
    case s.kind of
        RECTANGLE_TYPE: FillRectangle(s.FillColor, s.rect);
        CIRCLE_TYPE:    FillCircle(s.FillColor, s.circ);
        TRIANGLE_TYPE:  FillTriangle(s.FillColor, s.tri);
        ELLIPSE_TYPE:   FillEllipse(s.FillColor, s.ellipse);
        UNKNOWN_TYPE: exit;
    end;    //end switch
end;

// Draw the drawing
procedure DoDrawing(var d: Drawing);
var
    i: integer;
begin   
    // Clear screen and redraw menu
    ClearScreen(ColorWhite);
    DrawShapesMenu(d.SelectedShape);
    
    // Draw the shapes
    for i := 0 to MAX_SHAPES do
        DrawShape(d.Shapes[i]); //for i 
end;//do drawing

// ============================
// = Procedures to Add Shapes =
// ============================

// Add a Rectangle to the drawing
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

// Add a Circle to the drawing
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

// Add a Triangle to the drawing
procedure AddTriangle(var s: shape; pt:  point2d);
var
    pt1: Point2d;
begin
    // Set the type and color
    s.kind := TRIANGLE_TYPE;
    s.FillColor := ColorYellow;
    
    // Set point 1 to pt
    s.Tri[0] := pt;
    
    // Offset point -20 x, 50 y
    pt1.x := -20;
    pt1.y := 50;
    s.Tri[1] := PointAdd(pt, pt1);
    
    // Offset point 20 x, 50 y
    pt1.x := 20;
    s.Tri[2] := PointAdd(pt, pt1);
end;//add Triangle

procedure AddEllipse(var s: shape; pt: point2d);
begin
    // Setup the shape
    s.kind := ELLIPSE_TYPE;
    s.FillColor := ColorGreen;
    
    // Copy in menu ellipse
    s.ellipse := MENU_ELLIPSE;
    
    // Change shapes position
    s.ellipse.x := pt.x;
    s.ellipse.y := pt.y;    
end;//add ellipse

//input processing procedures
function ProcessKey(var d: drawing): boolean;
var
    i: integer;
begin
    if(KeyDown(VK_Q)) then
        result := true;
        exit;
    if(KeyDown(VK_C)) then
    begin
        d.Index := 0;
        for i := 0 to MAX_SHAPES do
            d.Shapes[i].kind := UNKNOWN_TYPE;
    end;
    result := false;    
end;

// Check if the user has clicked in a shape in the toolbar
procedure ProcessMenuClick(var d: drawing; pt: point2d);
begin   
    if PointInRect(pt, MENU_RECT) then
        d.SelectedShape := RECTANGLE_TYPE;
        
    if PointInTriangle(pt, MENU_TRIANGLE) then
        d.SelectedShape := TRIANGLE_TYPE;

    if PointInCircle(pt, MENU_CIRCLE) then
        d.SelectedShape := CIRCLE_TYPE;
        
    if PointInRect(pt, MENU_ELLIPSE) then
        d.SelectedShape := ELLIPSE_TYPE;
end;

// Add a shape to the canvas
procedure ProcessCanvasClick(var d: drawing; pt: point2d);
begin
    // Try to add a shape... is the current index < maximum?
    if(d.Index < MAX_SHAPES) then
    begin
        // Select the shape to add...
        case(d.SelectedShape) of
            RECTANGLE_TYPE: AddRectangle(d.Shapes[d.Index], pt);
            CIRCLE_TYPE:    AddCircle(d.Shapes[d.Index], pt);
            TRIANGLE_TYPE:  AddTriangle(d.Shapes[d.Index], pt);
            ELLIPSE_TYPE:   AddEllipse(d.Shapes[d.Index], pt);
            else exit;
        end;    //end case
        d.Index := d.Index + 1;
    end;    //end if
end;    //process pad click

//the input procedure
function ProcessInput(var d: Drawing): boolean;
var
    pt: point2d;
    keyPressed: boolean;
begin
    if(MouseClicked(LeftButton)) then
    begin
        pt := MousePosition();
        
        if pt.x < 60 then
            ProcessMenuClick(d, pt)
        else            
            ProcessCanvasClick(d, pt);
    end;//if LEFT BUTTON
    
    if(AnyKeyPressed()) then  
        keyPressed := ProcessKey(d)
    else
        keyPressed := false;
        
    result := keyPressed;
end;//process input

procedure SetupDrawing(var d: Drawing);
var
    i: integer;
begin
    // Start adding at index 0
    d.Index := 0;
    
    // All shapes are unknown...
    for i := 0 to MAX_SHAPES do 
        d.Shapes[i].kind := UNKNOWN_TYPE;
end;

//main  
procedure Main();
var
    quit: boolean;
    myDrawing: Drawing;
begin
    // Create the drawing...
    quit := false;
    
    // Initialise the drawing with empty data...
    SetupDrawing(myDrawing);
            
    OpenGraphicsWindow('Draw Shapes', 800, 500);
    
    repeat
        ProcessEvents(); // read user interactions...
        quit := ProcessInput(myDrawing);
        
        DoDrawing(myDrawing);
        RefreshScreen();
    until (WindowCloseRequested() or quit);
     
    ReleaseAllResources();
end;

begin
    Main();
end.