#include <X/mit-copyright.h>

/* $Header: XQueryInput.c,v 10.1 86/03/25 18:05:05 jg Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
Status XQueryInput (w, mask)
	Window w;
	long *mask;
{
	register Display *dpy;
	register XReq *req;
	XRep rep;

	GetReq(X_QueryWindow, w);

	if (!_XReply(dpy, &rep))
	    return (0);
	*mask = rep.param.l[4];
	return (1);
}

