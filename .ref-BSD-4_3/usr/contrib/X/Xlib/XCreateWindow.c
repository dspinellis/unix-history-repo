#include <X/mit-copyright.h>

/* $Header: XCreateWindow.c,v 10.4 86/02/01 15:31:38 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
Window XCreateWindow (parent, x, y, width, height, bdr_width, border, bgnd)
	int x, y, width, height, bdr_width;
	Window parent;
	Pixmap border, bgnd;
{
	register Display *dpy;
	register XReq *req;
	XRep rep;

	GetReq(X_CreateWindow, parent);
	req->param.s[0] = height;
	req->param.s[1] = width;
	req->param.s[2] = x;
	req->param.s[3] = y;
	req->param.l[2] = border;
	req->param.l[3] = bgnd;
	req->func = bdr_width;
	if (!_XReply(dpy, &rep))
	    return(NULL);
	return (rep.param.l[0]);
}

