#include <X/mit-copyright.h>

/* Copyright 	Massachusetts Institute of Technology  1985 */
/* $Header: XParseGeom.c,v 10.9 86/02/01 15:37:45 tony Rel $ */
#include "XlibInternal.h"

/* 
 *Returns pointer to first char ins search which is also in what, else NULL.
 */
static char *strscan (search, what)
char *search, *what;
{
	int i, len = strlen (what);
	char c;

	while ((c = *(search++)) != NULL)
	for (i = 0; i < len; i++)
	if (c == what [i]) return (--search);
	return (NULL);
}

/*
 *    XParseGeometry parses strings of the form
 *   "=<width>x<height>{+-}<xoffset>{+-}<yoffset>", where
 *   width, height, xoffset, and yoffset are unsigned integers.
 *   Example:  "=80x24+300-49"
 *   The equal sign is optional.
 *   It returns a bitmask that indicates which of the four values
 *   were actually found in the string.  For each value found,
 *   the corresponding argument is updated;  for each value
 *   not found, the corresponding argument is left unchanged. 
 */

int XParseGeometry (string, x, y, width, height)
char *string;
int *x, *y, *width, *height;    /* RETURN */
{
	int mask = NoValue;
	char *strind;
	char *index();

	if ( (string == NULL) || (*string == '\0')) return(mask);
	if (*string == '=')
	string++;  /* ignore possible '=' at beginning of geometry spec */

	strind = string;
	if (*strind != '+' && *strind != '-' && *strind != 'x') {
		*width = atoi (strind);
		mask |= WidthValue;
	}

	if (strind = index (string, 'x')) {
		*height = atoi (++strind);
		mask |= HeightValue;
	}
	else strind = string;

	if (strind = strscan (strind, "+-")) {
		if (*strind == '-') mask |= XNegative;
		*x = atoi (strind++);
		mask |= XValue;
		if (strind = strscan (strind, "+-")) {
			if (*strind == '-') mask |= YNegative;
			*y = atoi (strind);
			mask |= YValue;
		}
	}
	return (mask);
}

