program Lights; 
uses
    sgTypes, sgInput, sgGraphics, sgUtils, sgImages, sgGeometry;

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

