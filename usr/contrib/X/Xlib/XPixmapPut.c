#include <X/mit-copyright.h>

/* $Header: XPixmapPut.c,v 10.5 86/02/01 15:38:25 tony Rel $ */
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
	req->param.s[0] = height;
	req->param.s[1] = width;
	req->param.s[2] = source_x;
	req->param.s[3] = source_y;
	req->param.l[2] = pixmap;
	req->param.s[6] = dest_x;
	req->param.s[7] = dest_y;
	return;
}

