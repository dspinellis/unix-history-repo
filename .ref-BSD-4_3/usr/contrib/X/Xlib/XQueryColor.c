#include <X/mit-copyright.h>

/* $Header: XQueryColor.c,v 10.4 86/02/01 15:38:47 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
XQueryColor (color)
    	register Color *color;
{
	register Display *dpy;
	register XReq *req;
	XRep rep;

	GetReq (X_QueryColor, 0);
	req->param.u[0] = color->pixel;
	_XReply (dpy, &rep);
	color->red = rep.param.u[0];
    	color->green = rep.param.u[1];
    	color->blue = rep.param.u[2];
}

