#include <X/mit-copyright.h>

/* $Header: XDrawFilled.c,v 10.6 86/04/22 15:24:31 jg Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
XDrawFilled (w, vlist, vcount, pixel, func, planes)
	Window w;
	int func;
	int pixel, planes, vcount;
	Vertex *vlist;
{
	register Display *dpy;
	register XReq *req;
	int nbytes;

	GetReq(X_DrawFilled, w);
	dpy->lastdraw = (caddr_t) req;
	req->func = func;
	req->mask = planes;
	req->params0 = vcount;
	req->paramu1 = pixel;
	req->param.l[1] = 0;  /* means "no tile--use pixel" */
	nbytes = vcount*psizeof(Vertex);
	PackData(dpy, vlist, nbytes);
}

