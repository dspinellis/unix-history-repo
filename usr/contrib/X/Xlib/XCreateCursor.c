#include <X/mit-copyright.h>

/* $Header: XCreateCursor.c,v 10.5 86/02/01 15:31:11 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
Cursor XCreateCursor (width, height, cursor, mask, xOffset, yOffset,
    foreground, background, func)
    	short *cursor, *mask;
	int width, height;
	int xOffset, yOffset;
	int func;
	int foreground, background;
{
	register Display *dpy;
	register XReq *req;
	XRep rep;
	Bitmap cbitmap, mbitmap = 0;
	int nbytes = BitmapSize (width, height);
	Status status;

	GetReq(X_StoreBitmap, 0);
	req->param.s[0] = height;
	req->param.s[1] = width;
	Data (dpy, (char *)cursor, nbytes);

	if (mask) {
	    GetReq(X_StoreBitmap, 0);
	    req->param.s[0] = height;
	    req->param.s[1] = width;
	    Data (dpy, (char *)mask, nbytes);
	    dpy->request--; /* to synchronize properly with XError packets */
	    }

	cbitmap = _XReply (dpy, &rep) ? rep.param.l[0] : 0;

	if (mask) {
	    dpy->request++;
	    mbitmap = _XReply (dpy, &rep) ? rep.param.l[0] : 0;
	    }

	if (!cbitmap || (mask && !mbitmap)) {
	    /* an error occurred. Clean up and return. */
	    if (cbitmap)
	    	XFreeBitmap (cbitmap);
	    if (mbitmap)
	    	XFreeBitmap (mbitmap);
	    return (0);
	    }

	GetReq(X_StoreCursor, 0);
	req->func = func;
	req->param.l[0] = cbitmap;
	req->param.s[2] = foreground;
	req->param.s[3] = background;
	req->param.l[2] = mbitmap;
	req->param.s[6] = xOffset;
	req->param.s[7] = yOffset;
	status = _XReply (dpy, &rep);
	XFreeBitmap (cbitmap);
	if (mbitmap) 
	    XFreeBitmap (mbitmap);
	return (status ? rep.param.l[0] : 0);
}

