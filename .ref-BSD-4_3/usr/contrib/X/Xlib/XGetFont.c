#include <X/mit-copyright.h>

/* $Header: XGetFont.c,v 10.4 86/02/01 15:34:52 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
Font XGetFont (name)
	char *name;
{
	register XReq *req;
	Display *dpy;
	XRep rep;
	int len;

	GetReq(X_GetFont, 0);
	req->param.s[0] = len = strlen (name);
	Data(dpy, name, len);
	if (!_XReply(dpy, &rep))
	    return(NULL);
	return (rep.param.l[0]);
}

