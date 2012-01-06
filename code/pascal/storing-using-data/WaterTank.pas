program GameMain;
uses
  sgTypes, sgInput, sgAudio, sgGraphics, sgResources, sgUtils, sgText;
  
  const
    MAX_HEIGHT = 400;
    MAX_WIDTH = 200;

procedure DrawWaterTank(x, y, width, height: Integer; pctFull: Single);
var
  ellipseHeight: Single;
  bodyHeight, bodyY: Single;
  bottomEllipseY, topEllipseY: Single;
  waterHeight, waterY: Single;
begin
  ellipseHeight := height * 0.1;
  bodyHeight := height - ellipseHeight;  // the area for the center of the cylinder
  bodyY := y + ellipseHeight / 2;
  bottomEllipseY := (y + height) - ellipseHeight;

  waterHeight := pctFull * bodyHeight;
  waterY := bodyY + (bodyHeight - waterHeight);
  topEllipseY := waterY - ellipseHeight / 2;
  
  // Water...
  // Bottom ellipse
  FillEllipse(ColorBlue, x, bottomEllipseY, width, Round(ellipseHeight));
  DrawEllipse(ColorBlack, x, bottomEllipseY, width, Round(ellipseHeight));
  // Body - center of cylinder
  FillRectangle(ColorBlue, x, waterY, width, Round(waterHeight));
  //Top ellipse
  FillEllipse(ColorBlue, x, topEllipseY, width, Round(ellipseHeight));
  DrawEllipse(ColorBlack, x, topEllipseY, width, Round(ellipseHeight));

  // Frame
  DrawEllipse(ColorBlack, x, y, width, Round(ellipseHeight));
  DrawLine(ColorBlack, x, y + ellipseHeight / 2, x, bottomEllipseY + ellipseHeight / 2);
  DrawLine(ColorBlack, x + width, y + ellipseHeight / 2, x + width, bottomEllipseY + ellipseHeight / 2);
end;

procedure Main();
begin
  OpenGraphicsWindow('Water Tanks', 800, 600);
  
  ClearScreen(ColorWhite);
  DrawWaterTank(10, 50, 100, 200, 0.75);
  DrawWaterTank(150, 50, 100, 300, 0.0);
  DrawWaterTank(300, 50, 70, 100, 0.25);
  DrawWaterTank(450, 50, Round(rnd() * MAX_HEIGHT), Round(rnd() * MAX_WIDTH), 0.25);
  RefreshScreen();

  Delay(5000);
  
  ReleaseAllResources();
end;

begin
  Main();
end.
