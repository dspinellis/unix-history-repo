#include <X/mit-copyright.h>

/* $Header: XDrawTiled.c,v 10.6 86/04/22 15:23:22 jg Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
XDrawTiled (w, vlist, vcount, tile, func, planes)
	Window w;
	int func;
	int planes, vcount;
	Pixmap tile;
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
	req->param.l[1] = tile;
	nbytes = vcount*psizeof(Vertex);
	PackData(dpy, vlist, nbytes);
}

