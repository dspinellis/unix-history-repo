#include <X/mit-copyright.h>

/* $Header: XPixSet.c,v 10.5 86/04/22 15:19:21 jg Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
XPixSet (w, x, y, width, height, pixel)
	Window w;
	int x, y;
	int pixel;
	int width, height;
{
	register Display *dpy;
	register XReq *req;

	GetReq(X_PixFill, w);
	req->mask = ~0;  /* all planes */
	req->func = GXcopy;
	req->params0 = height;
	req->params1 = width;
	req->params2 = x;
	req->params3 = y;
	req->params4 = pixel;
	req->param.l[3] = 0;  /* no mask bitmap */
}

