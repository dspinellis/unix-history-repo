#include <X/mit-copyright.h>

/* Copyright 	Massachusetts Institute of Technology  1985 */
/* $Header: XPutBackEvent.c,v 10.5 86/02/01 15:38:34 tony Rel $ */
/* XPutBackEvent puts an event back at the head of the queue. */

#include "XlibInternal.h"

extern _QEvent *_qfree;

XPutBackEvent (event)
	register XEvent *event;
	{
	register _QEvent *qelt;
	register Display *dpy = _XlibCurrentDisplay;
	if (!_qfree) {
    	    _qfree = (_QEvent *) malloc (sizeof (_QEvent));
	    _qfree->next = NULL;
	    }
	qelt = _qfree;
	_qfree = qelt->next;
	qelt->next = dpy->head;
	qelt->event = *event;
	dpy->head = qelt;
	if (dpy->tail == NULL)
	    dpy->tail = qelt;
	dpy->qlen++;
	}
