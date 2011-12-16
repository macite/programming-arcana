program AssignmentTest;

procedure Main();
var
    myData: Integer = 10;
    daysInMonth, daysRemaining: Integer;
begin
    WriteLn('myData is ', myData);
    
    myData := myData + 1;    //add 1 to myData and store in myData
    WriteLn('myData is ', myData);  
    
    myData += 1;    //add 1 to myData and store in myData
    WriteLn('myData is ', myData);  
    
    myData *= 2;    //double myData and store in myData
    WriteLn('myData is ', myData);
    
    daysInMonth := 365 div 12; //assign daysInMonth a calculated value 
    WriteLn('On average there are ', daysInMonth, ' days in a month.');  
    
    //assign daysRemaining a calculated value
    daysRemaining := 365 - daysInMonth * 12;
    WriteLn('The remaining ', daysRemaining, 
            'days are distributed to a few months.');
end;

begin
    Main();
end.