#include <X/mit-copyright.h>

/* $Header: XBitmapBitPut.c,v 10.5 86/02/01 15:29:40 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
XBitmapBitsPut (w, x, y, width, height, data, 
    foreground, background, mask, func, planes)
	Window w;
	int x, y, width, height;
	short *data;
	int foreground, background;
	Bitmap mask;
	int func;
	int planes;
{
	register Display *dpy;
	register XReq *req;
	int len;

	GetReq(X_BitmapBitsPut, w);
	req->mask = planes;
	req->func = func;
	req->param.s[0] = height;
	req->param.s[1] = width;
	req->param.s[2] = x;
	req->param.s[3] = y;
	req->param.u[4] = foreground;
	req->param.u[5] = background;
	req->param.l[3] = mask;
	len = BitmapSize (width, height);
	Data (dpy,(char *)data, len);
}

