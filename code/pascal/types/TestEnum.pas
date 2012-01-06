program TestEnum;

type
    WarningLevel = ( SAFE, DANGER, EXTREME_DANGER );

procedure Main();
var
    situation: WarningLevel = SAFE;
begin
    case situation of
        case SAFE: WriteLn('Safe'); break;
        case DANGER: WriteLn('Danger!'); break;
        case EXTREME_DANGER: WriteLn('Run!'); break;
        else WriteLn('Unknown...');
    end;
end;

begin
    Main();
end.