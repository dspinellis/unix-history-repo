#include <X/mit-copyright.h>

/* $Header: XDrawTiled.c,v 10.5 86/02/01 15:32:42 tony Rel $ */
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
	req->param.s[0] = vcount;
	req->param.l[1] = tile;
	nbytes = vcount*sizeof(Vertex);
	Data(dpy, (char *) vlist, nbytes);
}

