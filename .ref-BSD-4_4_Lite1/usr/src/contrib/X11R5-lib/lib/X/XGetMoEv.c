/* $XConsortium: XGetMoEv.c,v 11.20 91/01/06 11:46:11 rws Exp $ */
/* Copyright    Massachusetts Institute of Technology    1986	*/

/*
Permission to use, copy, modify, distribute, and sell this software and its
documentation for any purpose is hereby granted without fee, provided that
the above copyright notice appear in all copies and that both that
copyright notice and this permission notice appear in supporting
documentation, and that the name of M.I.T. not be used in advertising or
publicity pertaining to distribution of the software without specific,
written prior permission.  M.I.T. makes no representations about the
suitability of this software for any purpose.  It is provided "as is"
without express or implied warranty.
*/

#define NEED_REPLIES
#include "Xlibint.h"

XTimeCoord *XGetMotionEvents(dpy, w, start, stop, nEvents)
    register Display *dpy;
    Time start, stop;
    Window w;
    int *nEvents;  /* RETURN */
{       
    xGetMotionEventsReply rep;
    register xGetMotionEventsReq *req;
    XTimeCoord *tc = NULL;
    long nbytes;
    LockDisplay(dpy);
    GetReq(GetMotionEvents, req);
    req->window = w;
/* XXX is this right for all machines? */
    req->start = start;
    req->stop  = stop;
    if (!_XReply (dpy, (xReply *)&rep, 0, xFalse)) {
	UnlockDisplay(dpy);
        SyncHandle();
	return (NULL);
	}
    
    if (rep.nEvents) {
	if (! (tc = (XTimeCoord *)
	       Xmalloc( (unsigned) 
		       (nbytes = (long) rep.nEvents * sizeof(XTimeCoord))))) {
	    _XEatData (dpy, (unsigned long) nbytes);
	    UnlockDisplay(dpy);
	    SyncHandle();
	    return (NULL);
	}
    }

    *nEvents = rep.nEvents;
    nbytes = SIZEOF (xTimecoord);
    {
	register XTimeCoord *tcptr;
	register int i;
	xTimecoord xtc;

	for (i = rep.nEvents, tcptr = tc; i > 0; i--, tcptr++) {
	    _XRead (dpy, (char *) &xtc, nbytes);
	    tcptr->time = xtc.time;
	    tcptr->x    = cvtINT16toShort (xtc.x);
	    tcptr->y    = cvtINT16toShort (xtc.y);
	}
    }

    UnlockDisplay(dpy);
    SyncHandle();
    return (tc);
}

