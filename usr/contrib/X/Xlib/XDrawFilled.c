#include <X/mit-copyright.h>

/* $Header: XDrawFilled.c,v 10.5 86/02/01 15:32:30 tony Rel $ */
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
	req->param.s[0] = vcount;
	req->param.u[1] = pixel;
	req->param.l[1] = 0;  /* means "no tile--use pixel" */
	nbytes = vcount*sizeof(Vertex);
	Data(dpy, (char *) vlist, nbytes);
}

