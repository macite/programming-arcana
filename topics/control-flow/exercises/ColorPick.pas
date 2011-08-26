procedure DrawColoredRects();
begin
  repeat
    ProcessEvents();
    ClearScreen();
    
    if KeyDown(vk_R) then FillRectangle(ColorRed, 10, 10, 780, 580);
    if KeyDown(vk_G) then FillRectangle(ColorGreen, 20, 20, 760, 560);
    if KeyDown(vk_B) then FillRectangle(ColorBlue, 30, 30, 740, 540);

    RefreshScreen();
  until KeyTyped(vk_ESCAPE);
end;