/*
 * Copyright (c) 1984, 1985 Xerox Corp.
 *
 * common definitons
 */

#define MICAS_PER_INCH	2540
#define POINTS_PER_INCH	72	/* these are fat points */


unsigned char *GetStringProp();
unsigned char *GetIntegerProp();

#define Make16BitChar(a, b)	((a << 8) | b)

int DebugLevel;
