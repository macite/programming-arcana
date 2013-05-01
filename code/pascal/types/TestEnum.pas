program TestEnum;

type
    WarningLevel = ( SAFE, DANGER, EXTREME_DANGER );

procedure Main();
var
    situation: WarningLevel = SAFE;
begin
    case situation of
        SAFE: WriteLn('Safe');
        DANGER: WriteLn('Danger!');
        EXTREME_DANGER: WriteLn('Run!');
        else WriteLn('Unknown...');
    end;
end;

begin
    Main();
end.