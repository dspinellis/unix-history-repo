#include <X/mit-copyright.h>

/* $Header: XPixmapSave.c,v 10.4 86/02/01 15:38:30 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
Pixmap XPixmapSave (w, x, y, width, height)
	Window w;
	int x, y, width, height;
{
	register Display *dpy;
	register XReq *req;
	XRep rep;

	GetReq(X_PixmapSave, w);
	req->param.s[0] = height;
	req->param.s[1] = width;
	req->param.s[2] = x;
	req->param.s[3] = y;
	if (!_XReply(dpy, &rep))
	    return(NULL);
	return (rep.param.l[0]);
}

