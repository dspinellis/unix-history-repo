#include <X/mit-copyright.h>

/* $Header: XGeom.c,v 10.6 86/08/04 09:52:35 wesommer Rel $/
/* Copyright Massachusetts Institute of Technology 1985 */

#include "XlibInternal.h"

/*
 * This routine given a user supplied positional argument and a default
 * argument (fully qualified) will return the position the window should take
 * returns 0 if there was some problem, else the position bitmask.
 */

int XGeometry (pos, def, bwidth, fwidth, fheight, xadd, yadd, x, y, width, height)
char *pos;				/* user provided geometry spec */
char *def;				/* default geometry spec for window */
int bwidth;				/* border width */
int fwidth, fheight;			/* size of position units */
int xadd, yadd;				/* any additional interior space */
register *x, *y, *width, *height;	/* always set on successful RETURN */
{
	int px, py, pwidth, pheight;	/* returned values from parse */
	int dx, dy, dwidth, dheight;	/* default values from parse */
	int pmask, dmask;		/* values back from parse */

	pmask = XParseGeometry(pos, &px, &py, &pwidth, &pheight);
	dmask = XParseGeometry(def, &dx, &dy, &dwidth, &dheight);

	/* set default values */
	*x = (dmask & XNegative) ? 
	    DisplayWidth()  + dx - dwidth * fwidth - 2 * bwidth - xadd : dx;
	*y = (dmask & YNegative) ? 
	    DisplayHeight() + dy - dheight * fheight - 2 * bwidth - yadd : dy;
	*width  = dwidth;
	*height = dheight;

	if (pmask & WidthValue)  *width  = pwidth;
	if (pmask & HeightValue) *height = pheight;

	if (pmask & XValue)
	    *x = (pmask & XNegative) ?
	      DisplayWidth()  + px - *width * fwidth - 2 * bwidth - xadd : px;
	if (pmask & YValue)
	    *y = (pmask & YNegative) ?
	      DisplayHeight() + py - *height * fheight - 2 * bwidth - yadd: py;

	return (pmask);
}
