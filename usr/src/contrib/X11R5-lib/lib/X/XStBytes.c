/* $XConsortium: XStBytes.c,v 11.20 91/01/08 14:41:56 gildea Exp $ */
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

/* insulate predefined atom numbers from cut routines */
static Atom n_to_atom[8] = { 
	XA_CUT_BUFFER0,
	XA_CUT_BUFFER1,
	XA_CUT_BUFFER2,
	XA_CUT_BUFFER3,
	XA_CUT_BUFFER4,
	XA_CUT_BUFFER5,
	XA_CUT_BUFFER6,
	XA_CUT_BUFFER7};

XRotateBuffers (dpy, rotate)
    register Display *dpy;
    int rotate;
{
	XRotateWindowProperties(dpy, RootWindow(dpy, 0), n_to_atom, 8, rotate);
}
    
char *XFetchBuffer (dpy, nbytes, buffer)
    register Display *dpy;
    int *nbytes;
    register int buffer;
{
    Atom actual_type;
    int actual_format;
    unsigned long nitems;
    unsigned long leftover;
    unsigned char *data;
    *nbytes = 0;
    if ((buffer < 0) || (buffer > 7)) return (NULL);
/* XXX should be (sizeof (maxint) - 1)/4 */
    if (XGetWindowProperty(dpy, RootWindow(dpy, 0), n_to_atom[buffer], 
	0L, 10000000L, False, XA_STRING, 
	&actual_type, &actual_format, &nitems, &leftover, &data) != Success) {
	return (NULL);
	}
    if ( (actual_type == XA_STRING) &&  (actual_format != 32) ) {
	*nbytes = nitems;
	return((char *)data);
	}
    if ((char *) data != NULL) Xfree ((char *)data);
    return(NULL);
}

char *XFetchBytes (dpy, nbytes)
    register Display *dpy;
    int *nbytes;
{
    return (XFetchBuffer (dpy, nbytes, 0));
}

#if NeedFunctionPrototypes
XStoreBuffer (
    register Display *dpy,
    _Xconst char *bytes,
    int nbytes,
    register int buffer)
#else
XStoreBuffer (dpy, bytes, nbytes, buffer)
    register Display *dpy;
    char *bytes;
    int nbytes;
    register int buffer;
#endif
{
    if ((buffer < 0) || (buffer > 7)) return;
    XChangeProperty(dpy, RootWindow(dpy, 0), n_to_atom[buffer], 
	XA_STRING, 8, PropModeReplace, (unsigned char *) bytes, nbytes);
}

#if NeedFunctionPrototypes
XStoreBytes (
    register Display *dpy,
    _Xconst char *bytes,
    int nbytes)
#else
XStoreBytes (dpy, bytes, nbytes)
    register Display *dpy;
    char *bytes;
    int nbytes;
#endif
{
    XStoreBuffer (dpy, bytes, nbytes, 0);
}
