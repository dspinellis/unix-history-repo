#include <X/mit-copyright.h>

/* $Header: XGetResHint.c,v 10.4 86/02/01 15:35:16 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
Status XGetResizeHint (w, width0, height0, widthinc, heightinc)
	Window w;
	int *width0, *widthinc, *height0, *heightinc;
{
	register Display *dpy;
	register XReq *req;
	XRep rep;

	GetReq(X_GetResizeHint, w);
	if (!_XReply(dpy, &rep))
	    return (0);
	*height0 = rep.param.s[0];
	*heightinc = rep.param.s[1];
	*width0 = rep.param.s[2];
	*widthinc = rep.param.s[3];
	return (1);
}

