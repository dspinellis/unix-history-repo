/* $XConsortium: XStName.c,v 11.15 91/01/08 14:42:00 gildea Exp $ */
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

#include <X11/Xlibint.h>
#include <X11/Xatom.h>

#if NeedFunctionPrototypes
XStoreName (
    register Display *dpy,
    Window w,
    _Xconst char *name)
#else
XStoreName (dpy, w, name)
    register Display *dpy;
    Window w;
    char *name;
#endif
{
    XChangeProperty(dpy, w, XA_WM_NAME, XA_STRING, 
		8, PropModeReplace, (unsigned char *)name,
                name ? strlen(name) : 0);
}

#if NeedFunctionPrototypes
XSetIconName (
    register Display *dpy,
    Window w,
    _Xconst char *icon_name)
#else
XSetIconName (dpy, w, icon_name)
    register Display *dpy;
    Window w;
    char *icon_name;
#endif
{
    XChangeProperty(dpy, w, XA_WM_ICON_NAME, XA_STRING, 
		8, PropModeReplace, (unsigned char *)icon_name,
		icon_name ? strlen(icon_name) : 0);
}
