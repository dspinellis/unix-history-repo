#include <X/mit-copyright.h>

/* $Header: XPixSet.c,v 10.4 86/02/01 15:38:09 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
XPixSet (w, x, y, width, height, pixel)
	Window w;
	int x, y;
	int pixel;
	int width, height;
{
	register Display *dpy;
	register XReq *req;

	GetReq(X_PixFill, w);
	req->mask = ~0;  /* all planes */
	req->func = GXcopy;
	req->param.s[0] = height;
	req->param.s[1] = width;
	req->param.s[2] = x;
	req->param.s[3] = y;
	req->param.s[4] = pixel;
	req->param.l[3] = 0;  /* no mask bitmap */
}

