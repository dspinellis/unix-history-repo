#include <X/mit-copyright.h>

/* $Header: XGetHardColor.c,v 10.4 86/02/01 15:34:57 tony Rel $ */
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
	req->param.u[0] = color->red;
	req->param.u[1] = color->green;
	req->param.u[2] = color->blue;
	status = _XReply (dpy, &rep);
	if (status)
	    color->pixel = rep.param.u[0];
	return (status);
}

