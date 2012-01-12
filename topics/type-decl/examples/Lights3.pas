// ======================
// = Main - Entry Point =
// ======================

procedure Main();
var 
    // Create a number of lights
    lights: array [0..NUM_LIGHTS-1] of Light;
begin
    OpenGraphicsWindow('Lights', 800, 600);
   
    LoadBitmaps();
    
    // Setup the lights
    lights[0] := CreateLight(true, SMALL_LIGHT, PointAt(10, 10));
    lights[1] := CreateLight(true, MEDIUM_LIGHT, PointAt(110, 10));
    lights[2] := CreateLight(true, LARGE_LIGHT, PointAt(210, 10));
    
    repeat 
        // Update
        ProcessEvents();
        UpdateLights(lights);
        
        //Draw
        ClearScreen();
        DrawLights(lights);
        RefreshScreen();
    until WindowCloseRequested();
    
    ReleaseAllResources();
end;

begin
    Main();
end.