#include <X/mit-copyright.h>

/* $Header: XFreeBitmap.c,v 10.4 86/02/01 15:34:02 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
XFreeBitmap (bitmap)
    	Bitmap bitmap;
{
	register Display *dpy;
	register XReq *req;

	GetReq(X_FreeBitmap, 0);
	req->param.l[0] = bitmap;
}

