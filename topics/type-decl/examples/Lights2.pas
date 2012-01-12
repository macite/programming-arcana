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
    result := PointInRect(mouse, 
                          BitmapRectangle(l.position.x, l.position.y, lightBmp));
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
