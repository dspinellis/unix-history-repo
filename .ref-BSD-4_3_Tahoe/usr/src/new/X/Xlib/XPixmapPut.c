#include <X/mit-copyright.h>

/* $Header: XPixmapPut.c,v 10.6 86/04/22 15:21:00 jg Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
XPixmapPut (w, source_x, source_y, dest_x, dest_y, 
width, height, pixmap, func, planes)
	Window w;
	int source_x, source_y, dest_x, dest_y, width, height;
	Pixmap pixmap;
	int func, planes;
{
	register Display *dpy;
	register XReq *req;

	GetReq(X_PixmapPut, w);
	req->mask = planes;
	req->func = func;
	req->params0 = height;
	req->params1 = width;
	req->params2 = source_x;
	req->params3 = source_y;
	req->param.l[2] = pixmap;
	req->params6 = dest_x;
	req->params7 = dest_y;
	return;
}

