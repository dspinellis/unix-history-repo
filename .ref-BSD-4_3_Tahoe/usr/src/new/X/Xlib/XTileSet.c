#include <X/mit-copyright.h>

/* $Header: XTileSet.c,v 10.4 86/02/01 15:41:11 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
XTileSet (w, x, y, width, height, tile)
	Window w;
	int x, y;
	int width, height;
	Pixmap tile;
{
	register Display *dpy;
	register XReq *req;

	GetReq(X_TileFill, w);
	req->mask = ~0;  /* all planes */
	req->func = GXcopy;
	req->param.s[0] = height;
	req->param.s[1] = width;
	req->param.s[2] = x;
	req->param.s[3] = y;
	req->param.l[2] = tile;
	req->param.l[3] = 0;  /* no clip mask */
}

