#include <X/mit-copyright.h>

/* $Header: XStoreBitmap.c,v 10.6 86/04/22 15:28:12 jg Rel $ */
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
	req->params0 = height;
	req->params1 = width;
	len = BitmapSize (width, height)/UBPS;
	PackData(dpy, data, len);
	if (!_XReply(dpy, &rep))
	    return(NULL);
	return (rep.param.l[0]);
}

