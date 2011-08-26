/* Program: lights (C++, SwinGame) */
#include <stdbool.h>
#include <strings.h>
#include <stdio.h>
#include "SwinGame.h"

// =====================
// = Delcare Constants =
// =====================

#define NUM_LIGHTS 3

// =================
// = Declare Types =
// =================

// There are three sizes of light...
typedef enum
{
    SMALL_LIGHT,
    MEDIUM_LIGHT,
    LARGE_LIGHT
} light_size;

// A light is on/off, have a size, and a position
typedef struct
{
    bool        is_on;      // is the light on?
    light_size  size;       // size of the light
    point2d     position;   // location of the light (top left)
} light;
