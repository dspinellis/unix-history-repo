/* $XConsortium: XSetStCmap.c,v 1.7 91/01/08 14:41:51 gildea Exp $ */

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

void XSetStandardColormap(dpy, w, cmap, property)
    Display *dpy;
    Window w;
    XStandardColormap *cmap;
    Atom property;		/* XA_RGB_BEST_MAP, etc. */
{
    Screen *sp;
    XStandardColormap stdcmap;

    sp = _XScreenOfWindow (dpy, w);
    if (!sp) {
	/* already caught the XGetGeometry error in _XScreenOfWindow */
	return;
    }

    stdcmap.colormap	= cmap->colormap;
    stdcmap.red_max	= cmap->red_max;
    stdcmap.red_mult	= cmap->red_mult;
    stdcmap.green_max	= cmap->green_max;
    stdcmap.green_mult  = cmap->green_mult;
    stdcmap.blue_max	= cmap->blue_max;
    stdcmap.blue_mult	= cmap->blue_mult;
    stdcmap.base_pixel	= cmap->base_pixel;
    stdcmap.visualid	= sp->root_visual->visualid;
    stdcmap.killid	= None;		/* don't know how to kill this one */

    XSetRGBColormaps (dpy, w, &stdcmap, 1, property);
    return;
}
