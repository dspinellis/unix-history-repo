#include <X/mit-copyright.h>

/* $Header: XSync.c,v 10.4 86/02/01 15:40:36 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"

extern _QEvent *_qfree;

/* Synchronize with errors and events, optionally discarding pending events. */

XSync (discard)
	int discard;  /* 0 or 1 */
{   	
    	register Display *dpy;
	register XReq *req;
	XRep rep;
	GetReq (X_SetUp, 0);
	_XReply(dpy, &rep);
	if (discard && dpy->head) {
	    dpy->tail->next = _qfree;
	    _qfree = dpy->head;
	    dpy->head = dpy->tail = NULL;
	    dpy->qlen = 0;
	    }
}

