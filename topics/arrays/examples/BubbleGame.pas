program BubbleGame;
uses
  sgTypes, sgInput, sgAudio, sgGraphics, sgResources, sgUtils, sgText, 
  sgSprites, sgImages;

const BUBBLE_COUNT = 10;

// Load the bubble image
procedure LoadResources();
begin
    LoadBitmapNamed('bubble', 'bubble.png');
end;

// Place a bubble somewhere on the screen, and give it a random movement
procedure PlaceBubble(bubble : Sprite);
begin
    SpriteSetX(bubble, Rnd(ScreenWidth() - SpriteWidth(bubble)));
    SpriteSetY(bubble, Rnd(ScreenHeight() - SpriteHeight(bubble)));
    SpriteSetDx(bubble, (Rnd() * 2) - 1); // between +1 and -1
    SpriteSetDy(bubble, (Rnd() * 2) - 1); // between +1 and -1
end;

// Create bubbles, and place them on the screen to start
procedure PopulateBubbles(var bubbles: array of Sprite);
var
    i, bubbleWidth, bubbleHeight: integer;
begin 
    bubbleWidth := BitmapWidth(BitmapNamed('bubbles'));
    bubbleHeight := BitmapHeight(BitmapNamed('bubbles'));
    
    for i := Low(bubbles) to High(bubbles) do
    begin
        bubbles[i] := CreateSprite(BitmapNamed('bubble'));
        PlaceBubble(bubbles[i]);
    end;
end;

// Update the bubble, move it and check if it is off screen
procedure UpdateBubble(bubble: Sprite);
begin
    UpdateSprite(bubble);  // Moves based on sprites dx,dy
    
    if (SpriteOffscreen(bubble)) then  // is it off screen?
        PlaceBubble(bubble)            // put it back on screen
end;

// Update all of the bubbles...
procedure UpdateBubbles(var bubbles: array of Sprite);
var
    i: integer;
begin    
    for i := Low(bubbles) to High(bubbles) do
    begin
        UpdateBubble(bubbles[i]);
    end;
end;