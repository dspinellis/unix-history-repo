#include <X/mit-copyright.h>

/* $Header: XQueryWidth.c,v 10.5 86/02/01 15:39:12 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"

int XQueryWidth (str, font)
	char *str;
	Font font;
{
	register Display *dpy;
	register XReq *req;
	XRep rep;
	int len;

	if (str == NULL) return(0);
	len = strlen (str);

	GetReq(X_StringWidth, 0);
	req->param.l[0] = font;
	req->param.s[2] = len;
	Data (dpy, str, len);
	if (!_XReply(dpy, &rep))
	    return(0);
	return(rep.param.s[0]);
}

