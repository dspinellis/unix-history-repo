#include <X/mit-copyright.h>

/* $Header: XScreenSaver.c,v 10.5 86/02/01 15:39:37 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"


XScreenSaver (save_timeout, pattern_timeout, video)
	int save_timeout, pattern_timeout, video;
{
	register Display *dpy;
	register XReq *req;

	GetReq(X_ScreenSaver, 0);
	req->func = video ? 1 : 0;
	req->param.s[0] = save_timeout;
	req->param.s[1] = pattern_timeout;
}

