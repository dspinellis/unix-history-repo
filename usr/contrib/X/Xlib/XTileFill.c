#include <X/mit-copyright.h>

/* $Header: XTileFill.c,v 10.4 86/02/01 15:41:05 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
XTileFill (w, x, y, width, height, tile, clipmask, func, planes)
	Window w;
	int x, y;
	int width, height;
	Pixmap tile;
	Bitmap clipmask;
	int func, planes;
{
	register Display *dpy;
	register XReq *req;

	GetReq(X_TileFill, w);
	req->mask = planes;
	req->func = func;
	req->param.s[0] = height;
	req->param.s[1] = width;
	req->param.s[2] = x;
	req->param.s[3] = y;
	req->param.l[2] = tile;
	req->param.l[3] = clipmask;
}

