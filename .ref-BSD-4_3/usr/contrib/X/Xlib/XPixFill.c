#include <X/mit-copyright.h>

/* $Header: XPixFill.c,v 10.4 86/02/01 15:38:05 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
XPixFill (w, x, y, width, height, pixel, clipmask, func, planes)
	Window w;
	int x, y;
	int pixel;
	int width, height;
	Bitmap clipmask;
	int func, planes;
{
	register Display *dpy;
	register XReq *req;

	GetReq(X_PixFill, w);
	req->mask = planes;
	req->func = func;
	req->param.s[0] = height;
	req->param.s[1] = width;
	req->param.s[2] = x;
	req->param.s[3] = y;
	req->param.u[4] = pixel;
	req->param.l[3] = clipmask;
}

