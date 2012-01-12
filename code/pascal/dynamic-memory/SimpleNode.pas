type 
    NodePtr = ^Node;
    Node = record
        value:  Integer; // node value
        next:   NodePtr; // pointer to next node
    end;

function CreateNode(val: Integer; next: NodePtr): NodePtr;
begin
    New(result);
    result^.value   := val;
    result^.next    := next;
end;

procedure Main();
var
    current: NodePtr;
begin
    current := CreateNode(0, nil);
    current := CreateNode(1, current);
end;

begin
    Main();
end.