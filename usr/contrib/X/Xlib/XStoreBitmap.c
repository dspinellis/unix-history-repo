#include <X/mit-copyright.h>

/* $Header: XStoreBitmap.c,v 10.5 86/02/01 15:39:52 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
Bitmap XStoreBitmap (width, height, data)
	int width, height;
	short *data;
{
    	register Display *dpy;
	register XReq *req;
	XRep rep;
	int len;

	GetReq(X_StoreBitmap, 0);
	req->param.s[0] = height;
	req->param.s[1] = width;
	len = BitmapSize (width, height);
	Data(dpy, (char *)data, len);
	if (!_XReply(dpy, &rep))
	    return(NULL);
	return (rep.param.l[0]);
}

