#include <X/mit-copyright.h>

/* $Header: XGetIconWind.c,v 10.4 86/02/01 15:35:07 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
Window XGetIconWindow (w)
	Window w;
{
	register Display *dpy;
	register XReq *req;
	XRep rep;

	GetReq(X_QueryWindow, w);
	if (!_XReply(dpy, &rep))
	    return(NULL);
	return (rep.param.b[11] == IsIcon ? w : rep.param.l[3]); 
	}
