#include <X/mit-copyright.h>

/* $Header: XQueryWindow.c,v 10.4 86/02/01 15:39:15 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
Status XQueryWindow (w, info)
	Window w;
	register WindowInfo *info;
{
	register Display *dpy;
	register XReq *req;
	XRep rep;

	GetReq(X_QueryWindow, w);

	if (!_XReply(dpy, &rep))
	    return (0);
	info->height = rep.param.s[0];
	info->width = rep.param.s[1];
	info->x = rep.param.s[2];
	info->y = rep.param.s[3];
	info->bdrwidth = rep.param.s[4];
	info->mapped = rep.param.b[10];
	info->type = rep.param.b[11];
	info->assoc_wind = rep.param.l[3];
	return (1);
}

