#include <X/mit-copyright.h>

/* $Header: XPending.c,v 10.4 86/02/01 15:37:51 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"

/* Read in any pending events and return the number of queued events. */

int XPending ()
{
	register int len;
	int pend;
	char buf[BUFSIZE];
	register XRep *rep;
	register Display *dpy = _XlibCurrentDisplay;
	
	_XFlush (dpy);
	if (ioctl(dpy->fd, FIONREAD, &pend) < 0)
	    _XIOError(dpy);
	if ((len = pend) < sizeof(XRep))
	    return(dpy->qlen);
	else if (len > BUFSIZE)
	    len = BUFSIZE;
	len /= sizeof(XRep);
	pend = len * sizeof(XRep);
	_XRead (dpy, buf, pend);
	for (rep = (XRep *) buf; len > 0; rep++, len--) {
	    if (rep->code == X_Error)
		_XError(dpy, (XErrorEvent *)rep);
	    else   /* must be an event packet */
		_XEnq(dpy, (XEvent *) rep);
	}
	return(dpy->qlen);
}

