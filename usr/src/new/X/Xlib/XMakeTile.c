#include <X/mit-copyright.h>

/* $Header: XMakeTile.c,v 10.4 86/02/01 15:36:44 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
Pixmap XMakeTile (pixel)
    	int pixel;
{
	register Display *dpy;
	register XReq *req;
	XRep rep;
	
	switch (pixel) {
	    case WhitePixel: return (_XlibCurrentDisplay->white);
	    case BlackPixel: return (_XlibCurrentDisplay->black);
	    }

	GetReq(X_MakePixmap, 0);
	req->param.l[0] = 0;
	req->param.s[2] = pixel;
	if (!_XReply (dpy, &rep))
	    return (NULL);
	return (rep.param.l[0]);
}

