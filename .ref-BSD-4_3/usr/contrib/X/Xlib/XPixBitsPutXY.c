#include <X/mit-copyright.h>

/* $Header: XPixBitsPutXY.c,v 10.5 86/02/01 15:37:55 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
XPixmapBitsPutXY (w, x, y, width, height, data, mask, func, planes)
	Window w;
	int x, y, width, height;
	short *data;
	Bitmap mask;
	int func;
	int planes;
{
	register Display *dpy;
	register XReq *req;
	int len;

	GetReq(X_PixmapBitsPut, w);
	req->mask = planes;
	req->func = func;
	req->param.s[0] = height;
	req->param.s[1] = width;
	req->param.s[2] = x;
	req->param.s[3] = y;
	req->param.s[4] = XYFormat;
	req->param.l[3] = mask;
	len = XYPixmapSize (width, height, dpy->dplanes);
	Data(dpy, (caddr_t)data, len);
}

