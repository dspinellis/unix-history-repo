#include <X/mit-copyright.h>

/* $Header: XPixBitsPutZ.c,v 10.4 86/02/01 15:38:00 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
XPixmapBitsPutZ (w, x, y, width, height, data, mask, func, planes)
	Window w;
	int x, y, width, height;
	caddr_t data;
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
	req->param.s[4] = ZFormat;
	req->param.l[3] = mask;
	len = (dpy->dplanes > 8)
	  ? WZPixmapSize (width, height)
	  : BZPixmapSize (width, height);
	Data (dpy, data, len);
}

