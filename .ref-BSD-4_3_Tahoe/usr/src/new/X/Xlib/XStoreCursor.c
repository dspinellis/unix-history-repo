#include <X/mit-copyright.h>

/* $Header: XStoreCursor.c,v 10.5 86/04/22 15:29:29 jg Rel $ */
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
	req->paramu2 = foreground;
	req->paramu3 = background;
	req->param.l[2] = mbitmap;
	req->params6 = xOffset;
	req->params7 = yOffset;
	if (!_XReply (dpy, &rep))
	    return (NULL);
	return (rep.param.l[0]);
}

