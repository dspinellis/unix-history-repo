/* $XConsortium: XGetStCmap.c,v 1.10 91/01/08 14:41:00 gildea Exp $ */

/***********************************************************
Copyright 1987 by Digital Equipment Corporation, Maynard, Massachusetts,
and the Massachusetts Institute of Technology, Cambridge, Massachusetts.

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the names of Digital or MIT not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.  

DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************/

#include <X11/Xlibint.h>
#include <X11/Xutil.h>
#include "Xatomtype.h"
#include <X11/Xatom.h>

/*
 * 				    WARNING
 * 
 * This is a pre-ICCCM routine.  It must not reference any of the new fields
 * in the XStandardColormap structure.
 */

Status XGetStandardColormap (dpy, w, cmap, property)
    Display *dpy;
    Window w;
    XStandardColormap *cmap;
    Atom property;		/* XA_RGB_BEST_MAP, etc. */
{
    Status stat;			/* return value */
    XStandardColormap *stdcmaps;	/* will get malloced value */
    int nstdcmaps;			/* count of above */

    stat = XGetRGBColormaps (dpy, w, &stdcmaps, &nstdcmaps, property);
    if (stat) {
	XStandardColormap *use;

	if (nstdcmaps > 1) {
	    VisualID vid;
	    Screen *sp = _XScreenOfWindow (dpy, w);
	    int i;

	    if (!sp) {
		if (stdcmaps) Xfree ((char *) stdcmaps);
		return False;
	    }
	    vid = sp->root_visual->visualid;

	    for (i = 0; i < nstdcmaps; i++) {
		if (stdcmaps[i].visualid == vid) break;
	    }

	    if (i == nstdcmaps) {	/* not found */
		Xfree ((char *) stdcmaps);
		return False;
	    }
	    use = &stdcmaps[i];
	} else {
	    use = stdcmaps;
	}
	    
	/*
	 * assign only those fields which were in the pre-ICCCM version
	 */
	cmap->colormap	 = use->colormap;
	cmap->red_max	 = use->red_max;
	cmap->red_mult	 = use->red_mult;
	cmap->green_max	 = use->green_max;
	cmap->green_mult = use->green_mult;
	cmap->blue_max	 = use->blue_max;
	cmap->blue_mult	 = use->blue_mult;
	cmap->base_pixel = use->base_pixel;

	Xfree ((char *) stdcmaps);	/* don't need alloced memory */
    }
    return stat;
}
