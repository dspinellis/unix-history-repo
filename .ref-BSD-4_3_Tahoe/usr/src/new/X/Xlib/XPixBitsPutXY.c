#include <X/mit-copyright.h>

/* $Header: XPixBitsPutXY.c,v 10.6 86/04/22 15:17:19 jg Rel $ */
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
	req->params0 = height;
	req->params1 = width;
	req->params2 = x;
	req->params3 = y;
	req->params4 = XYFormat;
	req->param.l[3] = mask;
	len = XYPixmapSize (width, height, dpy->dplanes);
	Data(dpy, (caddr_t)data, len);
}

