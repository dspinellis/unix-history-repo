/* $XConsortium: XLiProps.c,v 11.20 91/01/06 11:46:49 rws Exp $ */
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

#define NEED_REPLIES
#include "Xlibint.h"

Atom *XListProperties(dpy, window, n_props)
register Display *dpy;
Window window;
int *n_props;  /* RETURN */
{
    long nbytes;
    xListPropertiesReply rep;
    Atom *properties;
    register xResourceReq *req;

    LockDisplay(dpy);
    GetResReq(ListProperties, window, req);
    if (!_XReply(dpy, (xReply *)&rep, 0, xFalse)) {
	*n_props = 0;
	UnlockDisplay(dpy);
        SyncHandle();
	return ((Atom *) NULL);
    }

    if (rep.nProperties) {
	nbytes = rep.nProperties * sizeof(Atom);
	properties = (Atom *) Xmalloc ((unsigned) nbytes);
	nbytes = rep.nProperties << 2;
	if (! properties) {
	    _XEatData(dpy, (unsigned long) nbytes);
	    UnlockDisplay(dpy);
	    SyncHandle();
	    return (Atom *) NULL;
	}
	_XRead32 (dpy, (char *) properties, nbytes);
    }
    else properties = (Atom *) NULL;

    *n_props = rep.nProperties;
    UnlockDisplay(dpy);
    SyncHandle();
    return (properties);
}
