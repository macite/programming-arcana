//the input procedure
function ProcessInput(var d: Drawing): Boolean;
var
    pt: Point2D;
begin
    if MouseClicked(LeftButton) then
    begin
        pt := MousePosition();
        if pt.x < 60 then   ProcessMenuClick(d, pt)
        else                ProcessCanvasClick(d, pt);
    end; //if LEFT BUTTON
    
    if AnyKeyPressed() then result := ProcessKey(d)
    else result := false;
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

procedure Main();
var
    quit: Boolean;
    myDrawing: Drawing;
begin
    // Create the Drawing...
    quit := false;
    
    // Initialise the Drawing with empty data...
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