#include <X/mit-copyright.h>

/* $Header: XGetResHint.c,v 10.5 86/04/22 15:31:44 jg Rel $ */
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
	*height0 = rep.params0;
	*heightinc = rep.params1;
	*width0 = rep.params2;
	*widthinc = rep.params3;
	return (1);
}

