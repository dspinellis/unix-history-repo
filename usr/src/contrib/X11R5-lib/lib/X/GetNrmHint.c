/* $XConsortium: GetNrmHint.c,v 1.6 91/02/01 16:32:58 gildea Exp $ */
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
#include <X11/Xatom.h>
#include "Xatomtype.h"
#include <X11/Xutil.h>
#include <stdio.h>

Status XGetWMSizeHints (dpy, w, hints, supplied, property)
    Display *dpy;
    Window w;
    XSizeHints *hints;
    long *supplied;
    Atom property;
{
    xPropSizeHints *prop = NULL;
    Atom actual_type;
    int actual_format;
    unsigned long leftover;
    unsigned long nitems;

    if (XGetWindowProperty (dpy, w, property, 0L, 
			    (long)NumPropSizeElements,
			    False, XA_WM_SIZE_HINTS, &actual_type,
			    &actual_format, &nitems, &leftover, 
			    (unsigned char **)&prop)
	!= Success)
      return False;

    if ((actual_type != XA_WM_SIZE_HINTS) ||
	(nitems < OldNumPropSizeElements) || (actual_format != 32)) {
	if (prop != NULL) Xfree ((char *)prop);
	return False;
    }

    hints->flags	  = prop->flags;
    /* XSizeHints misdeclares these as int instead of long */
    hints->x = cvtINT32toInt (prop->x);
    hints->y = cvtINT32toInt (prop->y);
    hints->width = cvtINT32toInt (prop->width);
    hints->height = cvtINT32toInt (prop->height);
    hints->min_width  = cvtINT32toInt (prop->minWidth);
    hints->min_height = cvtINT32toInt (prop->minHeight);
    hints->max_width  = cvtINT32toInt (prop->maxWidth);
    hints->max_height = cvtINT32toInt (prop->maxHeight);
    hints->width_inc  = cvtINT32toInt (prop->widthInc);
    hints->height_inc = cvtINT32toInt (prop->heightInc);
    hints->min_aspect.x = cvtINT32toInt (prop->minAspectX);
    hints->min_aspect.y = cvtINT32toInt (prop->minAspectY);
    hints->max_aspect.x = cvtINT32toInt (prop->maxAspectX);
    hints->max_aspect.y = cvtINT32toInt (prop->maxAspectY);

    *supplied = (USPosition | USSize | PAllHints);
    if (nitems >= NumPropSizeElements) {
	hints->base_width= cvtINT32toInt (prop->baseWidth);
	hints->base_height= cvtINT32toInt (prop->baseHeight);
	hints->win_gravity= cvtINT32toInt (prop->winGravity);
	*supplied |= (PBaseSize | PWinGravity);
    }
    hints->flags &= (*supplied);	/* get rid of unwanted bits */
    Xfree((char *)prop);
    return True;
}


Status XGetWMNormalHints (dpy, w, hints, supplied)
    Display *dpy;
    Window w;
    XSizeHints *hints;
    long *supplied;
{
    return (XGetWMSizeHints (dpy, w, hints, supplied, XA_WM_NORMAL_HINTS));
}
