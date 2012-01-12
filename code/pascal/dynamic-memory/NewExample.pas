program NewExample;

procedure Main();
var
    ptr: ^Integer;
begin
    ptr := nil;
    
    // Allocate space on the heap...
    New(ptr);
    
    // Store a value on the heap (at locate allocated to ptr)
    ptr^ := 10;
    
    WriteLn('Ptr is ', HexStr(ptr));
    WriteLn('The value it points to is ', ptr^);
    
    Dispose(ptr);
    ptr := nil;
end;

begin
    Main();
end.