#include <X/mit-copyright.h>

/* $Header: XFeepControl.c,v 10.4 86/02/01 15:33:24 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"

/*
 * XFeepControl - Define the base volume for XFeep requests.  The volume
 * is in the range 0 to 7 with 7 being the loudest.
 */

XFeepControl(volume)
	int volume;
{
	register Display *dpy = _XlibCurrentDisplay;
	register XReq *req;

	GetReq(X_FeepControl, dpy->root);
	req->func = volume;
}

