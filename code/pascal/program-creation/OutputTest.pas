//
// OutputTest: Writes some messages to the Terminal.
//
program OutputTest;
uses SplashKit;

begin
  WriteLine('Output Test Program');
  Write(' 1 + 1 = ');
  WriteLine(1 + 1);
  Write(' Area of a circle with radius 3 = ');
  WriteLine(3.1415 * 3 * 3);
end.