#include <X/mit-copyright.h>

/* $Header: XFetchBuffer.c,v 10.4 86/02/01 15:33:29 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"

caddr_t XFetchBuffer (nbytes, buffer)
    	register int *nbytes;
	int buffer;
{
	register Display *dpy;
	register XReq *req;
	XRep rep;
	char *data;

	GetReq(X_FetchBytes, 0);
	req->func = buffer;
	if (!_XReply(dpy, &rep))
	    return(NULL);
	if ((*nbytes = rep.param.s[0]) == 0)
	    return (NULL);  /* empty cut buffer */
	if ((data = (char *) malloc(*nbytes)) == NULL) {
	    errno = ENOMEM;
	    _XIOError(dpy);
	    }
	_XReadPad (dpy, data, *nbytes);
	return(data);
}

