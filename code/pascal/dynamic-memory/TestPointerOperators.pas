type
    Point2D = record
        x, y: Single;
    end;
    
    Point2DPtr = ^Point2D;

procedure Main();
var
    my_x: Single = 0.0;
    pt: Point2D = (x: 1.0; y: 2.0);
    floatPtr: ^Single;
    ptPtr: Point2DPtr;
begin    
    floatPtr := @my_x;         // get pointer to my_x variable
    WriteLn(floatPtr^);        // print the value pointer to by floatPtr
    
    floatPtr := @pt.x;
    WriteLn(floatPtr^);        // print the value pointer to by floatPtr
    
    ptPtr = @pt;
    // follow pointer, and get x and y fields from what it points to...
    WriteLn(ptPtr^.x, ':', ptPtr^.y);
end;

begin
    Main();
end.