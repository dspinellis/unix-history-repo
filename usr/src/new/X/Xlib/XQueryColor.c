#include <X/mit-copyright.h>

/* $Header: XQueryColor.c,v 10.5 86/04/22 15:22:55 jg Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
XQueryColor (color)
    	register Color *color;
{
	register Display *dpy;
	register XReq *req;
	XRep rep;

	GetReq (X_QueryColor, 0);
	req->paramu0 = color->pixel;
	_XReply (dpy, &rep);
	color->red = rep.paramu0;
    	color->green = rep.paramu1;
    	color->blue = rep.paramu2;
}

