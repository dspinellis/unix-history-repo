/* $XConsortium: XFetchName.c,v 11.25 91/02/01 16:34:12 gildea Exp $ */
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
#include <X11/Xos.h>
#include <stdio.h>


Status XFetchName (dpy, w, name)
    register Display *dpy;
    Window w;
    char **name;
{
    Atom actual_type;
    int actual_format;
    unsigned long nitems;
    unsigned long leftover;
    unsigned char *data = NULL;
    if (XGetWindowProperty(dpy, w, XA_WM_NAME, 0L, (long)BUFSIZ, False, XA_STRING, 
	&actual_type,
	&actual_format, &nitems, &leftover, &data) != Success) {
        *name = NULL;
	return (0);
	}
    if ( (actual_type == XA_STRING) &&  (actual_format == 8) ) {

	/* The data returned by XGetWindowProperty is guarranteed to
	contain one extra byte that is null terminated to make retrieveing
	string properties easy. */

	*name = (char *)data;
	return(1);
	}
    if (data) Xfree ((char *)data);
    *name = NULL;
    return(0);
}

Status XGetIconName (dpy, w, icon_name)
    register Display *dpy;
    Window w;
    char **icon_name;
{
    Atom actual_type;
    int actual_format;
    unsigned long nitems;
    unsigned long leftover;
    unsigned char *data = NULL;
    if (XGetWindowProperty(dpy, w, XA_WM_ICON_NAME, 0L, (long)BUFSIZ, False,
        XA_STRING, 
	&actual_type,
	&actual_format, &nitems, &leftover, &data) != Success) {
        *icon_name = NULL;
	return (0);
	}
    if ( (actual_type == XA_STRING) &&  (actual_format == 8) ) {

	/* The data returned by XGetWindowProperty is guarranteed to
	contain one extra byte that is null terminated to make retrieveing
	string properties easy. */

	*icon_name = (char*)data;
	return(1);
	}
    if (data) Xfree ((char *)data);
    *icon_name = NULL;
    return(0);
}
