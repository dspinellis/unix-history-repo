#include <X/mit-copyright.h>

/* $Header: XChngBorder.c,v 10.4 86/02/01 15:29:59 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
XChangeBorder (w, tile)
	Window w;
	Pixmap tile;
{
	register Display *dpy;
	register XReq *req;

	GetReq (X_ChangeBorder, w);
	req->param.l[0] = tile;
}

