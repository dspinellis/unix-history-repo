#include <X/mit-copyright.h>

/* $Header: XDraw.c,v 10.5 86/02/01 15:32:19 tony Rel $ */
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
	req->param.s[0] = vcount;
	req->param.u[1] = pixel;
	req->param.b[4] = height;
	req->param.b[5] = width;
	req->param.s[3] = DrawSolidLine;
	nbytes = vcount*sizeof(Vertex);
	Data(dpy, (char *) vlist, nbytes);
}

