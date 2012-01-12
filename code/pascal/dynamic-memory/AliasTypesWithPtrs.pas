type 
    // The IntPtr type is a pointer to an integer
    IntPtr = ^Integer;
    
    // The five ints type is an array of five integers
    FiveInts = array [0..4] of Integer;
    
procedure Main();
var
    i: Integer;
    x: Integer = 10;
    // xPtr is an pointer to x
    xPtr: IntPtr; 
    // an array of five ints
    data: FiveInts = (0,1,2,3,4); 
begin    
    xPtr := @x;
    WriteLn(x, ' = ', xPtr^);
    
    for i := 0 to 4 do
    begin
        printf('data[', i, '] = ', data[i]);
    end;
end;

begin
    Main();
end;