#include <X/mit-copyright.h>

/* $Header: XGetNodes.c,v 10.2 86/02/01 15:35:11 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/
 
#include "XlibInternal.h"
#ifdef DNETCONN
#include <netdnet/dn.h>
#endif
 
struct dn_naddr *XGetNodes (nnodes)
    	int *nnodes;
{
#ifdef DNETCONN
	register Display *dpy;
	register XReq *req;
	XRep rep;
	char *buf;
	int nbytes;
 
	GetReq(X_GetHosts, 0);
	req->func = XAF_DECnet;
	if (!(_XReply (dpy, &rep)) || !(nbytes = rep.param.l[0])) {
	    /* error or empty list */
	    *nnodes = 0;
	    return (NULL);
	    }
	*nnodes = nbytes / sizeof (struct dn_naddr);
	if (!(buf = (char *) malloc (nbytes))) {
	    errno = ENOMEM;
	    _XIOError (dpy);
	    }
	_XReadPad (dpy, buf, nbytes);
	return ((struct dn_naddr *) buf);
#endif
}
