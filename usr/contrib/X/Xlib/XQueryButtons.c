#include <X/mit-copyright.h>

/* $Header: XQueryButtons.c,v 10.4 86/02/01 15:38:44 tony Rel $ */
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
	*x = rep.param.s[2];
	*y = rep.param.s[3];
	*subw = rep.param.l[0];
	*state = rep.param.s[4];
	return (1);
}

