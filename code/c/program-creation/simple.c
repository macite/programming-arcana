/*
* Program: simple.c
*
* Outputs some text for the user.
*
*/

int main()
{
  // Output some text...
  write_line("Programming can be great Fun!");
  write_line("");
  
  // Output the results of some calculations
  write_line("You can calculate things like 1 + 1 = ", to_string(1 + 1));
  write_line("Or 2 * PI * 3 = " + to_string( 2 * 3.1415 * 3 ) );
  
  // Finish off...
  write_line("Its great commanding the computer to perform actions for you!");
  return 0;
}