#include <X/mit-copyright.h>

/* $Header: XScreenSaver.c,v 10.6 86/04/22 15:27:55 jg Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"


XScreenSaver (save_timeout, pattern_timeout, video)
	int save_timeout, pattern_timeout, video;
{
	register Display *dpy;
	register XReq *req;

	GetReq(X_ScreenSaver, 0);
	req->func = video ? 1 : 0;
	req->params0 = save_timeout;
	req->params1 = pattern_timeout;
}

