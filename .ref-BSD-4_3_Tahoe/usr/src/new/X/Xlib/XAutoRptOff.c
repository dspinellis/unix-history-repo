#include <X/mit-copyright.h>

/* $Header: XAutoRptOff.c,v 10.4 86/02/01 15:29:33 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
XAutoRepeatOff ()
{
    	register Display *dpy;
	register XReq *req;

	GetReq(X_AutoRepeat, 0);
	req->func = 0;
}

