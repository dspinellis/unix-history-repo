#include <X/mit-copyright.h>

/* $Header: XQueryTile.c,v 10.5 86/04/22 15:26:04 jg Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
XQueryTileShape (width, height, rwidth, rheight)
    	int width, height, *rwidth, *rheight;
{
	register Display *dpy;
	register XReq *req;
	XRep rep;

	GetReq(X_QueryShape, 0);
	req->func = TileShape;
	req->params0 = height;
	req->params1 = width;
	_XReply (dpy, &rep);
	*rheight = rep.params0;
	*rwidth = rep.params1;
}

