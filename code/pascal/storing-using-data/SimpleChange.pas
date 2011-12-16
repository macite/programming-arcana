// Calculate the ideal change for a given transaction.
program SimpleChange;

const 
    TWO_DOLLARS = 200;
    ONE_DOLLAR = 100;
    FIFTY_CENTS = 50;
    TWENTY_CENTS = 20;
    TEN_CENTS = 10;
    FIVE_CENTS = 5;

function CoinsToGive(change, coinValue: Integer): Integer;
begin
    result := change div coinValue;    // integer division... ignore remainder
end;

procedure GiveChange(var changeValue: Integer; coinValue: Integer; coinDesc: String);
var
    toGive: Integer;
begin
    toGive := CoinsToGive(changeValue, coinValue);
    changeValue := changeValue - toGive * coinValue;
    
    Write(toGive, ' x ', coinDesc, ', ');
end;

function GetChangeValue(): Integer;
var
    costOfItem, payment: Integer;
begin
    Write('Cost of item (in cents): ');
    ReadLn(costOfItem);
    
    Write('Amount paid (in cents): ');
    ReadLn(payment);
    
    result := payment - costOfItem;
end;

procedure Main();
var
    changeValue: Integer;
begin
    changeValue := GetChangeValue();
    
    Write('Change: ');
    GiveChange(changeValue, TWO_DOLLARS,  '$2');
    GiveChange(changeValue, ONE_DOLLAR,   '$1');
    GiveChange(changeValue, FIFTY_CENTS,  '50c');
    GiveChange(changeValue, TWENTY_CENTS, '20c');
    GiveChange(changeValue, TEN_CENTS,    '10c');
    GiveChange(changeValue, FIVE_CENTS,   '5c');
    WriteLn();
end;

begin
    Main();
end.