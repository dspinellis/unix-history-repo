/* $XConsortium: XWindow.c,v 11.16 91/01/06 11:48:49 rws Exp $ */
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

#define AllMaskBits (CWBackPixmap|CWBackPixel|CWBorderPixmap|\
		     CWBorderPixel|CWBitGravity|CWWinGravity|\
		     CWBackingStore|CWBackingPlanes|CWBackingPixel|\
		     CWOverrideRedirect|CWSaveUnder|CWEventMask|\
		     CWDontPropagate|CWColormap|CWCursor)

Window XCreateWindow(dpy, parent, x, y, width, height, 
                borderWidth, depth, class, visual, valuemask, attributes)
    register Display *dpy;
    Window parent;
    int x, y;
    unsigned int width, height, borderWidth;
    int depth;
    unsigned int class;
    Visual *visual;
    unsigned long valuemask;
    XSetWindowAttributes *attributes;
{
    Window wid;
    register xCreateWindowReq *req;

    LockDisplay(dpy);
    GetReq(CreateWindow, req);
    req->parent = parent;
    req->x = x;
    req->y = y;
    req->width = width;
    req->height = height;
    req->borderWidth = borderWidth;
    req->depth = depth;
    req->class = class;
    if (visual == CopyFromParent)
	req->visual = CopyFromParent;
    else
	req->visual = visual->visualid;
    wid = req->wid = XAllocID(dpy);
    valuemask &= AllMaskBits;
    if (req->mask = valuemask) 
        _XProcessWindowAttributes (dpy, (xChangeWindowAttributesReq *)req, 
			valuemask, attributes);
    UnlockDisplay(dpy);
    SyncHandle();
    return (wid);
    }

_XProcessWindowAttributes (dpy, req, valuemask, attributes)
    register Display *dpy;
    xChangeWindowAttributesReq *req;
    register unsigned long valuemask;
    register XSetWindowAttributes *attributes;
    {

    /* Warning!  This code assumes that "unsigned long" is 32-bits wide */

    unsigned long values[32];
    register unsigned long *value = values;
    unsigned int nvalues;

    if (valuemask & CWBackPixmap)
	*value++ = attributes->background_pixmap;
	
    if (valuemask & CWBackPixel)
    	*value++ = attributes->background_pixel;

    if (valuemask & CWBorderPixmap)
    	*value++ = attributes->border_pixmap;

    if (valuemask & CWBorderPixel)
    	*value++ = attributes->border_pixel;

    if (valuemask & CWBitGravity)
    	*value++ = attributes->bit_gravity;

    if (valuemask & CWWinGravity)
	*value++ = attributes->win_gravity;

    if (valuemask & CWBackingStore)
        *value++ = attributes->backing_store;
    
    if (valuemask & CWBackingPlanes)
	*value++ = attributes->backing_planes;

    if (valuemask & CWBackingPixel)
    	*value++ = attributes->backing_pixel;

    if (valuemask & CWOverrideRedirect)
    	*value++ = attributes->override_redirect;

    if (valuemask & CWSaveUnder)
    	*value++ = attributes->save_under;

    if (valuemask & CWEventMask)
	*value++ = attributes->event_mask;

    if (valuemask & CWDontPropagate)
	*value++ = attributes->do_not_propagate_mask;

    if (valuemask & CWColormap)
	*value++ = attributes->colormap;

    if (valuemask & CWCursor)
	*value++ = attributes->cursor;

    req->length += (nvalues = value - values);

    nvalues <<= 2;			    /* watch out for macros... */
    Data32 (dpy, (long *) values, (long)nvalues);

    }
