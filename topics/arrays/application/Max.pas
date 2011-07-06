// Find the largest value in the array
function Max(const data: array of Double): Double;
var
  i: Integer;
begin
  result := data[Low(data)];
  for i := Low(data) + 1 to High(data) do
  begin
    if data[i] > result then result := data[i];
  end;
end;
