#include <X/mit-copyright.h>

/* $Header: XKeyClickCon.c,v 10.4 86/02/01 15:36:00 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"

/*
 * XKeyClickControl  -  Controls the volume of the "click" that the
 * keyboard makes when a key is pressed.  0 if "off", 8 is the loudest
 * volume.  Initially volume is set to 6.
 */

XKeyClickControl(volume)
	int volume;
{
	register XReq *req;
	register Display *dpy;

	GetReq(X_KeyClick, 0);
	req->func = volume;
}

