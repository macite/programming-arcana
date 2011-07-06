function Mean(const data: array of Double): Double;
begin
    result := Sum(data) / Length(data);
end;