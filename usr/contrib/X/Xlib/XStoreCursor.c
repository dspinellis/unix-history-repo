#include <X/mit-copyright.h>

/* $Header: XStoreCursor.c,v 10.4 86/02/01 15:40:17 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
Cursor XStoreCursor (cbitmap, mbitmap, xOffset, yOffset,
    foreground, background, func)
	Bitmap cbitmap, mbitmap;
	int xOffset, yOffset;
	int func;
	int foreground, background;
{
	register Display *dpy;
	register XReq *req;
	XRep rep;

	GetReq(X_StoreCursor, 0);
	req->func = func;
	req->param.l[0] = cbitmap;
	req->param.u[2] = foreground;
	req->param.u[3] = background;
	req->param.l[2] = mbitmap;
	req->param.s[6] = xOffset;
	req->param.s[7] = yOffset;
	if (!_XReply (dpy, &rep))
	    return (NULL);
	return (rep.param.l[0]);
}

