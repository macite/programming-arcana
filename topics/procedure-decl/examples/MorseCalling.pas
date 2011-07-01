// Program: MorseCalling - GameMain.pas
// Outputs audio for the CQ 'Calling Anyone' signal.
program MorseCalling;

procedure ShortSignal();
begin
    PlaySoundEffect('dit');
    Delay(200);
end;

procedure LongSignal();
begin
    PlaySoundEffect('dah');
    Delay(600);
end;

procedure SignalC();
begin
    Delay(600);
    LongSignal();
    ShortSignal();
    LongSignal();
    ShortSignal();
end;

procedure SignalQ();
begin
    Delay(600);
    LongSignal();
    LongSignal();
    ShortSignal();
    LongSignal();
end;

procedure LoadSounds();
begin
    LoadSoundEffectNamed('dah', 'dah.wav');
    LoadSoundEffectNamed('dit', 'dit.wav');
end;

procedure Main();
begin
    OpenAudio();
    LoadSounds();
    
    SignalC();
    SignalQ();
    
    CloseAudio();
    ReleaseAllResources();
end;

begin
    Main();
end.