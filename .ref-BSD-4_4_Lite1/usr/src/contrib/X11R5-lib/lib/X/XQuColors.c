/* $XConsortium: XQuColors.c,v 11.20 91/01/06 11:47:29 rws Exp $ */
/* Copyright    Massachusetts Institute of Technology    1986	*/

/*
Permission to use, copy, modify, distribute, and sell this software and its
documentation for any purpose is hereby granted without fee, provided that
the above copyright notice appear in all copies and that both that
copyright notice and this permission notice appear in supporting
documentation, and that the name of M.I.T. not be used in advertising or
publicity pertaining to distribution of the software without specific,
written prior permission.  M.I.T. makes no representations about the
suitability of this software for any purpose.  It is provided "as is"
without express or implied warranty.
*/

#define NEED_REPLIES
#include "Xlibint.h"

XQueryColors(dpy, cmap, defs, ncolors)
    register Display *dpy;
    Colormap cmap;
    XColor *defs; 		/* RETURN */
    int ncolors;
{
    register int i;
    xrgb *color;
    xQueryColorsReply rep;
    long nbytes;
    register xQueryColorsReq *req;

    LockDisplay(dpy);
    GetReq(QueryColors, req);

    req->cmap = cmap;
    req->length += ncolors; /* each pixel is a CARD32 */

    for (i = 0; i < ncolors; i++)
      Data32 (dpy, (long *)&defs[i].pixel, 4L);
       /* XXX this isn't very efficient */

    if (_XReply(dpy, (xReply *) &rep, 0, xFalse) != 0) {
	if (color = (xrgb *)
	    Xmalloc((unsigned) (nbytes = (long) ncolors * SIZEOF(xrgb)))) {

	    _XRead(dpy, (char *) color, nbytes);

	    for (i = 0; i < ncolors; i++) {
		register XColor *def = &defs[i];
		register xrgb *rgb = &color[i];
		def->red = rgb->red;
		def->green = rgb->green;
		def->blue = rgb->blue;
		def->flags = DoRed | DoGreen | DoBlue;
	    }
	    Xfree((char *)color);
	}
	else _XEatData(dpy, (unsigned long) nbytes);
    }
    UnlockDisplay(dpy);
    SyncHandle();
}

