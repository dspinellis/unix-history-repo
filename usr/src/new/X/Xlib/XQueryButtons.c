#include <X/mit-copyright.h>

/* $Header: XQueryButtons.c,v 10.5 86/04/22 15:22:41 jg Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
Status XQueryMouseButtons (w, x, y, subw, state)
	Window w;
	Window *subw;
	int *x, *y;
	short *state;
{
	register Display *dpy;
	register XReq *req;
	XRep rep;

	GetReq(X_QueryMouse, w);
	if (!_XReply(dpy, &rep))
	    return (0);
	*x = rep.params2;
	*y = rep.params3;
	*subw = rep.param.l[0];
	*state = rep.params4;
	return (1);
}

