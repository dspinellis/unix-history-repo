#include <X/mit-copyright.h>

/* $Header: XLine.c,v 10.4 86/02/01 15:36:05 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
XLine (w, x1, y1, x2, y2, width, height, pixel, func, planes)
	Window w;
	int pixel, width, height, func, planes;
	int x1, y1, x2, y2;
{
	register Display *dpy;
	register XReq *req;

	GetReq(X_Line, w);
	req->func = func;
	req->mask = planes;
	req->param.s[0] = x1;
	req->param.s[1] = y1;
	req->param.s[2] = x2;
	req->param.s[3] = y2;
	req->param.u[4] = pixel;
	req->param.b[10] = height;
	req->param.b[11] = width;
}

