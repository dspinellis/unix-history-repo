#include <X/mit-copyright.h>

/* $Header: XGetFont.c,v 10.5 86/04/22 15:29:34 jg Rel $ */
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
	req->params0 = len = strlen (name);
	Data(dpy, name, len);
	if (!_XReply(dpy, &rep))
	    return(NULL);
	return (rep.param.l[0]);
}

