#include <X/mit-copyright.h>

/* $Header: XMouseControl.c,v 10.4 86/02/01 15:37:10 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"


XMouseControl(accel, thresh)
	int accel, thresh;
{
	register Display *dpy;
	register XReq *req;

	GetReq(X_MouseControl, 0);
	req->param.s[0] = accel;
	req->param.s[1] = thresh;
}

