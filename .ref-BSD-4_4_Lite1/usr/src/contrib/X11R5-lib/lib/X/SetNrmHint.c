/* $XConsortium: SetNrmHint.c,v 1.3 91/01/08 14:39:55 gildea Exp $ */

/***********************************************************
Copyright 1988 by Wyse Technology, Inc., San Jose, Ca,
and the Massachusetts Institute of Technology, Cambridge, Massachusetts.
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

DIGITAL AND WYSE DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO
EVENT SHALL DIGITAL OR WYSE BE LIABLE FOR ANY SPECIAL, INDIRECT OR
CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF
USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
PERFORMANCE OF THIS SOFTWARE.

******************************************************************/

#include <X11/Xlibint.h>
#include <X11/Xutil.h>
#include "Xatomtype.h"
#include <X11/Xatom.h>
#include <X11/Xos.h>

void XSetWMSizeHints (dpy, w, hints, prop)
    Display *dpy;
    Window w;
    XSizeHints *hints;
    Atom prop;
{
    xPropSizeHints data;

    data.flags = (hints->flags & 
		  (USPosition|USSize|PPosition|PSize|PMinSize|PMaxSize|
		   PResizeInc|PAspect|PBaseSize|PWinGravity));

    /*
     * The x, y, width, and height fields are obsolete; but, applications
     * that want to work with old window managers might set them.
     */
    data.x = hints->x;
    data.y = hints->y;
    data.width = hints->width;
    data.height = hints->height;

    data.minWidth = hints->min_width;
    data.minHeight = hints->min_height;
    data.maxWidth  = hints->max_width;
    data.maxHeight = hints->max_height;
    data.widthInc = hints->width_inc;
    data.heightInc = hints->height_inc;
    data.minAspectX = hints->min_aspect.x;
    data.minAspectY = hints->min_aspect.y;
    data.maxAspectX = hints->max_aspect.x;
    data.maxAspectY = hints->max_aspect.y;
    data.baseWidth = hints->base_width;
    data.baseHeight = hints->base_height;
    data.winGravity = hints->win_gravity;
   
    XChangeProperty (dpy, w, prop, XA_WM_SIZE_HINTS, 32,
		     PropModeReplace, (unsigned char *) &data,
		     NumPropSizeElements);
}


void XSetWMNormalHints (dpy, w, hints)
    Display *dpy;
    Window w;
    XSizeHints *hints;
{
    XSetWMSizeHints (dpy, w, hints, XA_WM_NORMAL_HINTS);
}

