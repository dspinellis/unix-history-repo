/* $XConsortium: Depths.c,v 1.6 91/02/01 16:32:55 gildea Exp $ */
/* Copyright 1989 Massachusetts Institute of Technology */

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
#include <stdio.h>

/*
 * XListDepths - return info from connection setup
 */
int *XListDepths (dpy, scrnum, countp)
    Display *dpy;
    int scrnum;
    int *countp;
{
    Screen *scr;
    int count;
    int *depths;

    if (scrnum < 0 || scrnum >= dpy->nscreens) return NULL;

    scr = &dpy->screens[scrnum];
    if ((count = scr->ndepths) > 0) {
	register Depth *dp;
	register int i;

	depths = (int *) Xmalloc (count * sizeof(int));
	if (!depths) return NULL;
	for (i = 0, dp = scr->depths; i < count; i++, dp++) 
	  depths[i] = dp->depth;
    } else {
	/* a screen must have a depth */
	return NULL;
    }
    *countp = count;
    return depths;
}
