#include <X/mit-copyright.h>

/* $Header: XQueryWindow.c,v 10.5 86/04/22 15:27:44 jg Rel $ */
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
	info->height = rep.params0;
	info->width = rep.params1;
	info->x = rep.params2;
	info->y = rep.params3;
	info->bdrwidth = rep.params4;
	info->mapped = rep.param.b[10];
	info->type = rep.param.b[11];
	info->assoc_wind = rep.param.l[3];
	return (1);
}

