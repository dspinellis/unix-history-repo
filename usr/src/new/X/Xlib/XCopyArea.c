#include <X/mit-copyright.h>

/* $Header: XCopyArea.c,v 10.5 86/04/22 15:24:41 jg Rel $ */
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
	req->params0 = height;
	req->params1 = width;
	req->params2 = srcX;
	req->params3 = srcY;
	req->params6 = dstX;
	req->params7 = dstY;
}

