#include <X/mit-copyright.h>

/* $Header: XPixFill.c,v 10.5 86/04/22 15:18:31 jg Rel $ */
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
	req->params0 = height;
	req->params1 = width;
	req->params2 = x;
	req->params3 = y;
	req->paramu4 = pixel;
	req->param.l[3] = clipmask;
}

