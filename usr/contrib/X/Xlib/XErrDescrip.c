#include <X/mit-copyright.h>

/* $Header: XErrDescrip.c,v 10.4 86/02/01 15:32:52 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
#define num_error_codes 13

char *XErrorList[num_error_codes + 1] = {
	/* No error		*/	"",
	/* BadRequest		*/	"bad request code",
	/* BadValue		*/	"integer parameter out of range",
	/* BadWindow		*/	"parameter not a Window",
	/* BadPixmap		*/	"parameter not a Pixmap",
	/* BadBitmap		*/	"parameter not a Bitmap",
	/* BadCursor		*/	"parameter not a Cursor",
	/* BadFont		*/	"parameter not a Font",
	/* BadMatch		*/	"parameter mismatch",
	/* BadTile		*/	"Pixmap shape invalid for tiling",
	/* BadGrab		*/	"button/mouse already grabbed",
	/* BadAccess		*/	"access control violation",
	/* BadAlloc		*/	"insufficient resources",
    	/* BadColor 	    	*/  	"no such color",
};

char *XErrDescrip (code)
    register int code;
{
    if (code <= num_error_codes && code > 0)
	return (XErrorList[code]);
    return("Unknown error");
}

