#include <X/mit-copyright.h>

/* $Header: XBitmapBitPut.c,v 10.6 86/04/22 15:28:18 jg Rel $ */
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
	req->params0 = height;
	req->params1 = width;
	req->params2 = x;
	req->params3 = y;
	req->paramu4 = foreground;
	req->paramu5 = background;
	req->param.l[3] = mask;
	len = BitmapSize (width, height)/UBPS;
	PackData (dpy, data, len);
}

