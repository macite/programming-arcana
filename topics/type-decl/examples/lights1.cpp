// ====================================
// = Declare functions and procedures =
// ====================================

// Sets up a new light, and returns its data
light create_light(bool on, light_size sz, point2d pos)
{
    light result;
    
    result.is_on = on;          // sets the light on/off
    result.size = sz;           // sets the size of the light
    result.position = pos;      // sets its position
    
    return result;              // return the initialised light
}

// Get the bitmap to use for the light "l"
bitmap light_bitmap(light &l)
{
    char name[17] = ""; // construct the name of the bitmap... 16+1
    
    // the start of the name is based on the size of the bitmap
    switch (l.size)
    {
        case SMALL_LIGHT:
            strncat(name, "small light", 11);
            break;
        case MEDIUM_LIGHT:
            strncat(name, "medium light", 12);
            break;
        case LARGE_LIGHT:
            strncat(name, "large light", 11);
            break;
        default:
            return NULL;
    }
    
    // the end of the name is based on if the light is on/off
    if (l.is_on)
        strncat(name, " on", 4);
    else
        strncat(name, " off", 4);
    
    // return the bitmap with that name
    return bitmap_named(name);    
}

// Draw the light "l" to the screen
void draw_light(light &l)
{
    draw_bitmap(light_bitmap(l), l.position);
}