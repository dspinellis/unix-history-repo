/* $XConsortium: SetTxtProp.c,v 1.5 91/02/01 16:33:29 gildea Exp $ */
/***********************************************************
Copyright 1988 by Wyse Technology, Inc., San Jose, Ca.,
and the Massachusetts Institute of Technology, Cambridge, Massachusetts.

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the names of Wyse or MIT not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.  

WYSE DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************/

#include <X11/Xlibint.h>
#include <X11/Xatom.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>
#include <stdio.h>

void XSetTextProperty (dpy, w, tp, property)
    Display *dpy;
    Window w;
    Atom property;
    XTextProperty *tp;
{
    XChangeProperty (dpy, w, property, tp->encoding, tp->format,
		     PropModeReplace, tp->value, tp->nitems);
}

void XSetWMName (dpy, w, tp)
    Display *dpy;
    Window w;
    XTextProperty *tp;
{
    XSetTextProperty (dpy, w, tp, XA_WM_NAME);
}

void XSetWMIconName (dpy, w, tp)
    Display *dpy;
    Window w;
    XTextProperty *tp;
{
    XSetTextProperty (dpy, w, tp, XA_WM_ICON_NAME);
}

void XSetWMClientMachine (dpy, w, tp)
    Display *dpy;
    Window w;
    XTextProperty *tp;
{
    XSetTextProperty (dpy, w, tp, XA_WM_CLIENT_MACHINE);
}

