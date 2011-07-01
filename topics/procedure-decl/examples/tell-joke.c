/*
* Program: tell-joke.c
* Tell a joke using a number of procedures.
*/

void setup_joke();
{
  printf("Knock Knock.\n");
  printf("Who''s there?\n");
  printf("Boo\n");
  printf("Boo Who?\n");
}

void pause_for_dramatic_effect();
{
    printf(".. pause ..\n");
}

void punch_line();
{
  pause_for_dramatic_effect();
  printf("Don't cry its only a joke!\n");
}

void tell_joke();
{
  setup_joke();
  punch_line();
}

void ha();
{
  printf("ha");
}

void ha_ha();
{
  ha(); 
  ha();
}

void he_he();
{
  printf("he_he");
}

void ah();
{
  printf("ah");
}

void ha_hahe_he();
{
  ha_ha();
  he_he();
}

void laugh();
{
  ah();
  ha_ha();
  ha_hahe_he();
  pause_for_dramatic_effect();
  he_he(); 
}

void stop_laughing();
{
  printf("\n");
  ah();
  pause_for_dramatic_effect();
  printf("\n");
  printf("Yes, very funny :)\n");
}

void main();
{
  tell_joke();
  laugh();
  stop_laughing();
}