#include <X/mit-copyright.h>

/* $Header: XCreateWindow.c,v 10.5 86/04/22 15:27:38 jg Rel $ */
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
	req->params0 = height;
	req->params1 = width;
	req->params2 = x;
	req->params3 = y;
	req->param.l[2] = border;
	req->param.l[3] = bgnd;
	req->func = bdr_width;
	if (!_XReply(dpy, &rep))
	    return(NULL);
	return (rep.param.l[0]);
}

