procedure DrawColoredRectsV2();
begin
  repeat
    ProcessEvents();
    ClearScreen();
    
    if KeyDown(vk_R) then FillRectangle(ColorRed, 10, 10, 780, 580)
    else if KeyDown(vk_G) then FillRectangle(ColorGreen, 20, 20, 760, 560)
    else if KeyDown(vk_B) then FillRectangle(ColorBlue, 30, 30, 740, 540)
    else FillRectangle(ColorGrey, 40, 40, 720, 520);

    RefreshScreen();
  until KeyTyped(vk_ESCAPE) or WindowCloseRequested();
end;