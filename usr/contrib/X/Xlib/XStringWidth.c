#include <X/mit-copyright.h>

/* $Header: XStringWidth.c,v 10.4 86/02/01 15:40:32 tony Rel $ */
/* Copyright Massachusetts Institute of Technology 1985 */

#include "XlibInternal.h"
int XStringWidth (string, info, charpad, spacepad)
    register char *string;
    register FontInfo *info;
    register int charpad, spacepad;
    {
    register int result = 0;
    if (!*string)  /* zero length string */
    	return (0);

    if (info->fixedwidth) {
	int length = strlen (string);
	result = length * (info->width + charpad) - charpad;
           /* don't pad last character */
	if (spacepad) {
	    char *sub = string, *index();
	    while (sub = index (sub, ' ')) {
	    	result += spacepad;
		sub++;
	        }
    	    if (string[length-1] == ' ')
	    	result -= spacepad;
	        /* don't pad terminating space character */
	    }
	}

    else {   /* variable width font */
	unsigned char c;
	register short *widths = info->widths - info->firstchar;
	while (c = *(string++)) {
	    if ((c >= info->firstchar) && (c <= info->lastchar))
	    	result += (widths[c] + charpad);
	    if (c == ' ')
	    	result += spacepad;
	    }
	result -= charpad;  /* don't pad last character */
	if (*(string-1) == ' ')
	    result -= spacepad;
	    /* don't pad terminating space character */
	}

    return (result);
    }
