#include <X/mit-copyright.h>

/* $Header: XDraw.c,v 10.6 86/04/22 15:27:13 jg Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
XDraw (w, vlist, vcount, width, height, pixel, func, planes)
	Window w;
	int func;
	int pixel, planes, height, width, vcount;
	Vertex *vlist;
{
	register Display *dpy;
	register XReq *req;
	int nbytes;

	GetReq(X_Draw, w);
	dpy->lastdraw = (caddr_t)req;
	req->func = func;
	req->mask = planes;
	req->params0 = vcount;
	req->paramu1 = pixel;
	req->param.b[4] = height;
	req->param.b[5] = width;
	req->params3 = DrawSolidLine;
	nbytes = vcount*psizeof(Vertex);
	PackData(dpy, vlist, nbytes);
}

