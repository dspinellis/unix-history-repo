/* $XConsortium: XLiHosts.c,v 11.19 91/01/06 11:46:46 rws Exp $ */
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

/* This can really be considered an os dependent routine */

#define NEED_REPLIES
#include "Xlibint.h"
/*
 * can be freed using XFree.
 */

XHostAddress *XListHosts (dpy, nhosts, enabled)
    register Display *dpy;
    int *nhosts;	/* RETURN */
    Bool *enabled;	/* RETURN */
    {
    register XHostAddress *outbuf = 0, *op;
    xListHostsReply reply;
    long nbytes;
    unsigned char *buf, *bp;
    register unsigned i;
    register xListHostsReq *req;

    LockDisplay(dpy);
    GetReq (ListHosts, req);

    if (!_XReply (dpy, (xReply *) &reply, 0, xFalse)) {
       UnlockDisplay(dpy);
       SyncHandle();
       return (XHostAddress *) NULL;
    }

    if (reply.nHosts) {
	nbytes = reply.length << 2;	/* compute number of bytes in reply */
	op = outbuf = (XHostAddress *)
	    Xmalloc((unsigned) (nbytes + reply.nHosts * sizeof(XHostAddress)));

	if (! outbuf) {	
	    _XEatData(dpy, (unsigned long) nbytes);
	    UnlockDisplay(dpy);
	    SyncHandle();
	    return (XHostAddress *) NULL;
	}
	bp = buf = 
	    ((unsigned char  *) outbuf) + reply.nHosts * sizeof(XHostAddress);

	_XRead (dpy, (char *) buf, nbytes);

	for (i = 0; i < reply.nHosts; i++) {
	    op->family = ((xHostEntry *) bp)->family;
	    op->length =((xHostEntry *) bp)->length; 
	    op->address = (char *) (((xHostEntry *) bp) + 1);
	    bp += SIZEOF(xHostEntry) + (((op->length + 3) >> 2) << 2);
	    op++;
	}
    }

    *enabled = reply.enabled;
    *nhosts = reply.nHosts;
    UnlockDisplay(dpy);
    SyncHandle();
    return (outbuf);
}


    


