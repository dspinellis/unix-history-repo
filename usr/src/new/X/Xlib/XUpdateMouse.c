#include <X/mit-copyright.h>

/* $Header: XUpdateMouse.c,v 10.4 86/02/01 15:41:37 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"

extern _QEvent *_qfree;

/* Like XQueryMouse, but also reads pending events and flushes any MouseMoved
 * events at the head of the queue.  A good way to track the mouse is to use
 * a MouseMoved event as a "hint", by calling this routine to get up to date
 * coordinates.
 */

Status XUpdateMouse (w, x, y, subw)
	Window w;
	Window *subw;
	int *x, *y;
{
	register Display *dpy;
	register XReq *req;
	register _QEvent *qelt;
	XRep rep;

	XPending(dpy = _XlibCurrentDisplay);
	    /* read all events in socket's kernel buffer */
	GetReq(X_QueryMouse, w);
	if (!_XReply(dpy, &rep))
	    return(0);
	while ((qelt = dpy->head) && qelt->event.type == MouseMoved) {
	    if ((dpy->head = qelt->next) == NULL)
		dpy->tail = NULL;
	    qelt->next = _qfree;
	    _qfree = qelt;
	    dpy->qlen--;
	}
	*x = rep.param.s[2];
	*y = rep.param.s[3];
	*subw = rep.param.l[0];
	return (1);
}

