#include <X/mit-copyright.h>

/* $Header: XLine.c,v 10.5 86/04/22 15:30:18 jg Rel $ */
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
	req->params0 = x1;
	req->params1 = y1;
	req->params2 = x2;
	req->params3 = y2;
	req->paramu4 = pixel;
	req->param.b[10] = height;
	req->param.b[11] = width;
}

