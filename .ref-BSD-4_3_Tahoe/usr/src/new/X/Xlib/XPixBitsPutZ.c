#include <X/mit-copyright.h>

/* $Header: XPixBitsPutZ.c,v 10.5 86/04/22 15:18:02 jg Rel $ */
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
	req->params0 = height;
	req->params1 = width;
	req->params2 = x;
	req->params3 = y;
	req->params4 = ZFormat;
	req->param.l[3] = mask;
	len = (dpy->dplanes > 8)
	  ? WZPixmapSize (width, height)
	  : BZPixmapSize (width, height);
	Data (dpy, data, len);
}

