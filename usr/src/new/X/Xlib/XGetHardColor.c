#include <X/mit-copyright.h>

/* $Header: XGetHardColor.c,v 10.5 86/04/22 15:30:21 jg Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
Status XGetHardwareColor (color)
	register Color *color;
{
	register Display *dpy;
	register XReq *req;
	XRep rep;
	Status status;

	GetReq (X_GetColor, 0);
	req->paramu0 = color->red;
	req->paramu1 = color->green;
	req->paramu2 = color->blue;
	status = _XReply (dpy, &rep);
	if (status)
	    color->pixel = rep.paramu0;
	return (status);
}

