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
