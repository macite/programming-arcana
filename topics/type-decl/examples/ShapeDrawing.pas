program ShapeDrawer;
uses sgTypes, sgInput, sgAudio, sgGraphics, sgResources, sgGeometry;
    
// =====================
// = Declare Constants =
// =====================
const MAX_SHAPES = 20;

const MENU_RECT     : Rectangle = (x: 10; y: 250; width: 40; height: 50);
const MENU_CIRCLE   : Circle = (center: (x: 30; y: 100); radius: 20);
const DRAWING_PAD   : Rectangle = (x: 61; y: 0; width: 790; height: 600);
// Todo: Add Ellipse & Triangle

// =================
// = Declare Types =
// =================
type
    // The Shape Type is one of these options
    // Todo: add Ellipse & Triangle
    ShapeType = ( CIRCLE_TYPE, RECTANGLE_TYPE, UNKNOWN_TYPE ); 
    
    Shape = record                      // The shape records...
        fillColor: Color;               // Had a color, and 
        case kind: ShapeType of    // Shape Data is either...
            CIRCLE_TYPE:    ( circ: Circle;         );
            RECTANGLE_TYPE: ( rect: Rectangle;      );
            //Todo: Add Ellipse and Triangle
    end;
    
    Drawing = record                             // A Drawing records has
        Shapes: array[0..MAX_SHAPES-1] of shape; // a number of shapes
        Index: integer;                          // the index of the last shape
        SelectedShape: ShapeType;                // the selected shape type
    end;

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
    FillRectangle(ColorGreen, MENU_RECT);
    //Todo: Add Ellipse and Triangle
    
    // Redraw the selected shape
    case(SelectedShape) of
        RECTANGLE_TYPE: DrawRectangle(ColorBlack, MENU_RECT);
        CIRCLE_TYPE:    DrawCircle(ColorBlack, MENU_CIRCLE);
        //Todo: Add Ellipse and Triangle
    end;
end;