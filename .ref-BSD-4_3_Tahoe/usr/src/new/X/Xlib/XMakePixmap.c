#include <X/mit-copyright.h>

/* $Header: XMakePixmap.c,v 10.4 86/02/01 15:36:39 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
Pixmap XMakePixmap (bitmap, foreground, background)
    	Bitmap bitmap;
	int foreground, background;
{
	register Display *dpy;
	register XReq *req;
	XRep rep;

	GetReq(X_MakePixmap, 0);
	req->param.l[0] = bitmap;
	req->param.u[2] = foreground;
	req->param.u[3] = background;
	if (!_XReply (dpy, &rep))
	    return (NULL);
	return (rep.param.l[0]);
}

