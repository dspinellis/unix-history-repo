#include <X/mit-copyright.h>

/* $Header: XCopyArea.c,v 10.4 86/02/01 15:30:49 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
XCopyArea (w, srcX, srcY, dstX, dstY, width, height, func, planes)
	Window w;
	int func;
	int srcX, srcY, dstX, dstY, width, height;
	int planes;
{
	register Display *dpy;
	register XReq *req;

	GetReq(X_CopyArea, w);
	req->func = func;
	req->mask = planes;
	req->param.s[0] = height;
	req->param.s[1] = width;
	req->param.s[2] = srcX;
	req->param.s[3] = srcY;
	req->param.s[6] = dstX;
	req->param.s[7] = dstY;
}

