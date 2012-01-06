program GameMain;
uses
  sgTypes, sgInput, sgAudio, sgGraphics, sgResources, sgUtils, sgText;
  
const
  MAX_HEIGHT = 400; 
  MAX_WIDTH = 200;

procedure DrawWaterTank(x, y, width, height: Integer; pctFull: Single);
var
  ellipseHeight: Single;              // height of the ellipses for top/bottom
  bodyHeight, bodyY: Single;          // the height of the core of the cylinder
  bottomEllipseY, topEllipseY: Single;// the y position of the ellipses
  waterHeight, waterY: Single;        // the top (y) of the water and its height
begin
  ellipseHeight := height * 0.1;      // ellipse height = 10% total height
  bodyHeight := height - ellipseHeight;  // area for the center of the cylinder
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
  DrawLine(ColorBlack, x, y + ellipseHeight / 2, x, 
            bottomEllipseY + ellipseHeight / 2);
  DrawLine(ColorBlack, x + width, y + ellipseHeight / 2, x + width, 
            bottomEllipseY + ellipseHeight / 2);
end;
