#include <X/mit-copyright.h>

/* $Header: XLockToggle.c,v 10.4 86/02/01 15:36:10 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"


XLockToggle ()
{
	register Display *dpy;
	register XReq *req;

	GetReq(X_ShiftLock, 0);
	req->func = LockToggleMode;
}

