
// Draw all of the bubbles
procedure DrawBubbles(const bubbles: array of Sprite);
var
    i: integer;
begin    
    for i := Low(bubbles) to High(bubbles) do
    begin
        DrawSprite(bubbles[i]);
    end;
end;

// A start of a bubble game...
// Requires 'bubble.png' to be placed in Resources/images
procedure Main();
var
    // Create an array of bubbles
    bubbles: array [0 .. BUBBLE_COUNT - 1] of Sprite; 
begin
    OpenAudio();
    OpenGraphicsWindow('Bubble Pop!', 800, 600);
    
    LoadResources();
    PopulateBubbles(bubbles);    // Load the bubbles
    
    repeat
        // Update the game...
        ProcessEvents();
        UpdateBubbles(bubbles);
        
        // Draw the game
        ClearScreen();
        
        DrawFramerate(0,0);
        DrawBubbles(bubbles);
        
        RefreshScreen();
    until WindowCloseRequested();
    
    CloseAudio();
    
    ReleaseAllResources();
end;

begin
    Main();
end.