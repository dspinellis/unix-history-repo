#include <X/mit-copyright.h>

/* $Header: XMakeTiles.c,v 10.3 86/02/01 15:36:49 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"

int XMakeTiles(defs, ndefs)
    TileFrame defs[];
    register int ndefs;
{
    register int i;
    register int req_count = 0;
    register int res_count = 0;
    register Display *dpy = _XlibCurrentDisplay;
    register TileFrame *frame = defs;
    
    
    /*
     * Issue requests.
     */
    for (i = 0; i < ndefs; i++) {
	register XReq *req;
	switch (frame->pixel) {
	    case WhitePixel:
		frame->pixmap = dpy->white;
		break;
	    case BlackPixel:
		frame->pixmap = dpy->black;
		break;
	    default:
		GetReq(X_MakePixmap, 0);
		frame->pixmap = 0;
		req->param.l[0] = 0;
		req->param.s[2] = frame->pixel;
		req_count++;
		break;
	}
	/*
	 * Increment the frame pointer.
	 */
	frame++;
    }

    /*
     * Reset request number to its old value, so that
     * error packets are processed correctly.
     */
    dpy->request -= req_count;

    /*
     * Retrieve replies.
     */
    frame = defs;
    for (i = 0; i < ndefs; i++) {
	XRep rep;

	/*
	 * If the pixel was a degenerate case (white or black)
	 * then we already have the pixmap id, increment the
	 * result frame pointer and result count and continue.
	 */
	if (frame->pixmap != 0) {
	    res_count++;
	}
	else {
	    /*
	     * Increment request number so error packets
	     * are processed correctly.
	     */
	    dpy->request++;

	    /*
	     * Retrieve the reply.
	     */
	    if (!_XReply(dpy, &rep)) frame->pixmap = 0;
	    else {
		frame->pixmap = rep.param.l[0];
		res_count++;
	    }
	}
	/*
	 * Increment the frame pointer.
	 */
	frame++;
    }

    /*
     * Return the number of successful pixmaps.
     */
    return (res_count);
}

