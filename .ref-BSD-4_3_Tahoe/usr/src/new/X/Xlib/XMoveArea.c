#include <X/mit-copyright.h>

/* $Header: XMoveArea.c,v 10.4 86/02/01 15:37:15 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
XMoveArea (w, srcX, srcY, dstX, dstY, width, height)
	Window w;
	int srcX, srcY, dstX, dstY, width, height;
{
	register Display *dpy;
	register XReq *req;

	GetReq(X_CopyArea, w);
	req->func = GXcopy;
	req->mask = ~0;  /* all planes */
	req->param.s[0] = height;
	req->param.s[1] = width;
	req->param.s[2] = srcX;
	req->param.s[3] = srcY;
	req->param.s[6] = dstX;
	req->param.s[7] = dstY;
}

