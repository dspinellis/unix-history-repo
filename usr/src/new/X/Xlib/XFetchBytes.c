#include <X/mit-copyright.h>

/* $Header: XFetchBytes.c,v 10.5 86/04/22 15:28:57 jg Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"

caddr_t XFetchBytes (nbytes)
    	register int *nbytes;
{
	register Display *dpy;
	register XReq *req;
	XRep rep;
	char *data;

	GetReq(X_FetchBytes, 0);
	req->func = 0;  /* cut buffer 0 */
	if (!_XReply(dpy, &rep))
	    return(NULL);
	if ((*nbytes = rep.params0) == 0)
	    return (NULL);  /* empty cut buffer */
	if ((data = (char *) malloc(*nbytes)) == NULL) {
	    errno = ENOMEM;
	    _XIOError(dpy);
	    }
	_XReadPad (dpy, data, *nbytes);
	return(data);
}

