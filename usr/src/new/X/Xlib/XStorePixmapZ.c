#include <X/mit-copyright.h>

/* $Header: XStorePixmapZ.c,v 10.5 86/04/22 15:30:48 jg Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
Pixmap XStorePixmapZ (width, height, data)
	int width, height;
	caddr_t data;
{
    	register Display *dpy;
	register XReq *req;
	XRep rep;
	int len;

	GetReq(X_StorePixmap, 0);
	req->func = ZFormat;
	req->params0 = height;
	req->params1 = width;
	len = (dpy->dplanes > 8)
	  ? WZPixmapSize (width, height)
	  : BZPixmapSize (width, height);
	Data (dpy, data, len);
	if (!_XReply(dpy, &rep))
	    return(NULL);
	return (rep.param.l[0]);
}

