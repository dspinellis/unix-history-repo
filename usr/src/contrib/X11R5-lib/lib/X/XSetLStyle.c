/* $XConsortium: XSetLStyle.c,v 11.11 91/01/06 11:48:05 rws Exp $ */
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

#include "Xlibint.h"

XSetLineAttributes(dpy, gc, linewidth, linestyle, capstyle, joinstyle)
register Display *dpy;
GC gc;
unsigned int linewidth; /* CARD16 */
int linestyle;
int capstyle;
int joinstyle;
{
    XGCValues *gv = &gc->values;

    LockDisplay(dpy);
    if (linewidth != gv->line_width) {
	gv->line_width = linewidth;
	gc->dirty |= GCLineWidth;
    }
    if (linestyle != gv->line_style) {
	gv->line_style = linestyle;
	gc->dirty |= GCLineStyle;
    }
    if (capstyle != gv->cap_style) {
	gv->cap_style = capstyle;
	gc->dirty |= GCCapStyle;
    }
    if (joinstyle != gv->join_style) {
	gv->join_style = joinstyle;
	gc->dirty |= GCJoinStyle;
    }
    UnlockDisplay(dpy);
    SyncHandle();
}
