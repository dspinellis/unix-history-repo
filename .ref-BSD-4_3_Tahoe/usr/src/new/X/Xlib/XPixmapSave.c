#include <X/mit-copyright.h>

/* $Header: XPixmapSave.c,v 10.5 86/04/22 15:21:23 jg Rel $ */
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
	req->params0 = height;
	req->params1 = width;
	req->params2 = x;
	req->params3 = y;
	if (!_XReply(dpy, &rep))
	    return(NULL);
	return (rep.param.l[0]);
}

