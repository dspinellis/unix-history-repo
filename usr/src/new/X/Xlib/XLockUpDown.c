#include <X/mit-copyright.h>

/* $Header: XLockUpDown.c,v 10.4 86/02/01 15:36:15 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"


XLockUpDown ()
{
	register Display *dpy;
	register XReq *req;

	GetReq(X_ShiftLock, 0);
	req->func = LockUpDownMode;
}

