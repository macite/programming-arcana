#include "splashkit.h"

#include <string>
using namespace std;

#define STROUHAL 0.33

int main()
{
    string bird_name, line;
    double freq, amp;

    // read in the details from the user
    write("What is the name of the bird: ");
    bird_name = read_line();

    write("What is the frequency of its wing stroke? (beats per second) Frequency: ");
    line = read_line();
    freq = convert_to_double(line);

    write("What is the amplitude of its wings? (meters) Amplitude: ");
    line = read_line();
    amp = convert_to_double(line);

    // calculate the air speed, given a fixed Strouhal value
    double result;
    result = freq * amp / STROUHAL;

    // output the airspeed of the bird
    string output;
    output = "Bird " + bird_name + " - f: " + to_string(freq) + ", A: " + to_string(amp) + ". Speed: " + to_string(result);
    write_line(output);

    return 0;
}
