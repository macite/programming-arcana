program Lights; 
uses
    sgTypes, sgInput, sgGraphics, sgResources, sgImages, sgGeometry;

// =====================
// = Delcare Constants =
// =====================

const NUM_LIGHTS = 3;

// =================
// = Declare Types =
// =================

type 
    // There are three sizes of light...
    LightSize = (SMALL_LIGHT, MEDIUM_LIGHT, LARGE_LIGHT);
    
    // A light is on/off, have a size, and a position
    Light = record
        isOn:       Boolean;        // is the light on?
        size:       LightSize;      // size of the light
        position:   Point2d;        // location of the light (top left)
    end;

// ====================================
// = Declare functions and procedures =
// ====================================

// Sets up a new light, and returns its data
function CreateLight(switchOn: boolean; sz: LightSize; pos: Point2d): Light;
var
    checkLight : Light;
begin    
    checkLight.isOn := switchOn;    // sets the light on/off
    checkLight.size := sz;          // sets the size of the light
    result.position := pos;         // sets its position
    
    result := checkLight;           // return the initialised light
end;

// Get the bitmap to use for the light 'l'
function LightBitmap(const one: Light): bitmap;
var
    name: String;
begin
    // the start of the name is based on the size of the bitmap
    case(one.size) of
        SMALL_LIGHT: name := 'small light';
        MEDIUM_LIGHT: name := 'medium light';
        LARGE_LIGHT: name := 'large light';
    else
        result := nil;
        exit;
    end;
    
    // the end of the name is based on if the light is on/off
    if (one.isOn) then name += ' on'
    else name += ' off';
    
    // return the bitmap with that name
    result := BitmapNamed(name);    
end;

// Draw the light 'l' to the screen
procedure DrawLight(const l: Light);
begin
    DrawBitmap(LightBitmap(l), l.position);
end;

// Draw all of the lights in 'lights'
procedure DrawLights(const lights: array of Light);
var
    i: integer;
begin
    for i := Low(lights) to High(lights) do
    begin
        DrawLight(lights[i]);
    end;
end;

// Is the light currently under the mouse?
function LightUnderMouse(var l: light): boolean;
var
    mouse: Point2d;
    lightBmp: Bitmap;
begin
    // get the mouse position
    mouse := MousePosition();
    // get the light bitmap, to determine its size etc.
    lightBmp := LightBitmap(l);
    
    // Simple version using a bounded rectangle
    result := PointInRect(mouse, BitmapRectangle(l.position.x, l.position.y, lightBmp));
end;

// Check if the lights have been changed (clicked)
procedure UpdateLights(var lights: array of Light);
var
    i: integer;
begin
    // only change if the mouse was clicked
    if ( MouseClicked(LeftButton) ) then
    begin
        // for all of the lights
        for i:= Low(lights) to High(lights) do
        begin
            // if the light is under the mouse
            if (LightUnderMouse(lights[i])) then
                // change state (on = off, off = on)
                lights[i].isOn := not lights[i].isOn;
        end;
    end;
end;

// Load all of the bitmaps name is based on 'size' + 'state'
procedure LoadBitmaps();
begin
    // Load 'on' lights
    LoadBitmapNamed('small light on', 'on_sml.png');
    LoadBitmapNamed('medium light on', 'on_med.png');
    LoadBitmapNamed('large light on', 'on.png');
    
    // Load 'off' lights
    LoadBitmapNamed('small light off', 'off_sml.png');
    LoadBitmapNamed('medium light off', 'off_med.png');
    LoadBitmapNamed('large light off', 'off.png');    
end;

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