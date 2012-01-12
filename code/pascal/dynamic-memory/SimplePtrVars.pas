program PointerVariables;

type
    IntPtr = ^Integer;

procedure PrintIntPointer(ptr: IntPtr);
begin
    WriteLn(HexStr(ptr), '=>', ptr^);
end;

procedure Main();
var
    i, j: Integer;
    p: IntPtr;
    ptrs: array [0..1] of ^Integer;
begin
    ptrs[0] := @i;
    ptrs[1] := @j;
    
    Write('Enter values for i and j: ');
    ReadLn(i, j);
    
    p := @i;
    PrintIntPointer(p);
    PrintIntPointer(@i);
    PrintIntPointer(@j);
    PrintIntPointer(ptrs[1]);
end;

begin
    Main();
end.