#include <X/mit-copyright.h>

/* $Header: XGetHosts.c,v 10.4 86/02/01 15:35:02 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
struct in_addr *XGetHosts (nhosts)
    	int *nhosts;
{
	register Display *dpy;
	register XReq *req;
	XRep rep;
	char *buf;
	int nbytes;

	GetReq(X_GetHosts, 0);
	req->func = XAF_INET;
	if (!(_XReply (dpy, &rep)) || !(nbytes = rep.param.l[0])) {
	    /* error or empty list */
	    *nhosts = 0;
	    return (NULL);
	    }
	*nhosts = nbytes / sizeof (struct in_addr);
	if (!(buf = (char *) malloc (nbytes))) {
	    errno = ENOMEM;
	    _XIOError (dpy);
	    }
	_XReadPad (dpy, buf, nbytes);
	return ((struct in_addr *) buf);
}

