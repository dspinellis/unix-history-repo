#include <X/mit-copyright.h>

/* Copyright Massachusetts Institute of Technology 1985 */

/* dbm database, mapping color names (strings) to RGB values */
/* $Header: rgb.h,v 10.6 86/02/01 15:17:17 tony Rel $ */

#define RGB_DB "/usr/lib/rgb"

typedef struct _rgb {unsigned short red, green, blue;} RGB;
