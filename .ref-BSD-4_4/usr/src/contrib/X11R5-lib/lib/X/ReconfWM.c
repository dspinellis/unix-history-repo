/* $XConsortium: ReconfWM.c,v 1.4 91/01/06 11:43:47 rws Exp $ */
/* Copyright    Massachusetts Institute of Technology    1986   */

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

#define NEED_EVENTS
#define NEED_REPLIES
#include "Xlibint.h"

#define AllMaskBits (CWX|CWY|CWWidth|CWHeight|\
                     CWBorderWidth|CWSibling|CWStackMode)

static int restack_error_handler();

Status XReconfigureWMWindow (dpy, w, screen, mask, changes)
    register Display *dpy;
    Window w;
    int screen;
    unsigned int mask;
    XWindowChanges *changes;
{
    XConfigureRequestEvent ev;
    Window root = RootWindow (dpy, screen);
    Bool succeeded;

    /*
     * Only need to go through the trouble if we are actually changing the
     * stacking mode.
     */
    if (!(mask & CWStackMode)) {
	XConfigureWindow (dpy, w, mask, changes);
	return True;
    }


    /*
     * We need to inline XConfigureWindow and XSync so that everything is done
     * while the display is locked.
     */

    LockDisplay(dpy);
    /*
     * Remember the old error handler, set the new one, set the sequence
     * number to look for, clear the failure flag, try the request, and
     * restore the error handler.
     */
    dpy->reconfigure_wm_window.old_handler = _XErrorFunction;
    _XErrorFunction = restack_error_handler;
    dpy->reconfigure_wm_window.sequence_number = NextRequest(dpy);
    dpy->reconfigure_wm_window.succeeded = True;


    /*
     * XConfigureWindow (dpy, w, mask, changes);
     */
    {
	unsigned long values[7];
	register unsigned long *value = values;
	long nvalues;
	register xConfigureWindowReq *req;

	GetReq(ConfigureWindow, req);
	req->window = w;
	mask &= AllMaskBits;
	req->mask = mask;

	/* Warning!  This code assumes that "unsigned long" is 32-bits wide */
	if (mask & CWX) *value++ = changes->x;
	if (mask & CWY) *value++ = changes->y;
	if (mask & CWWidth) *value++ = changes->width;
	if (mask & CWHeight) *value++ = changes->height;
	if (mask & CWBorderWidth) *value++ = changes->border_width;
	if (mask & CWSibling) *value++ = changes->sibling;
	if (mask & CWStackMode) *value++ = changes->stack_mode;
	req->length += (nvalues = value - values);
	nvalues <<= 2;			/* watch out for macros... */
	Data32 (dpy, (long *) values, nvalues);
    }

    /*
     * XSync (dpy, 0)
     */
    {
	xGetInputFocusReply rep;
	register xReq *req;

	GetEmptyReq(GetInputFocus, req);
	(void) _XReply (dpy, (xReply *)&rep, 0, xTrue);
    }

    /*
     * Put the world back together; copy the failure code out so that it can
     * be accessed once the display is unlocked.
     */
    _XErrorFunction = dpy->reconfigure_wm_window.old_handler;
    succeeded = dpy->reconfigure_wm_window.succeeded;
    dpy->reconfigure_wm_window.old_handler = NULL;
    dpy->reconfigure_wm_window.sequence_number = 0;
    dpy->reconfigure_wm_window.succeeded = False;

    UnlockDisplay(dpy);
    SyncHandle();


    /*
     * If the request succeeded, then everything is okay; otherwise, send event
     */
    if (succeeded) return True;

    ev.type		= ConfigureRequest;
    ev.window		= w;
    ev.parent		= root;
    ev.value_mask	= (mask & AllMaskBits);
    ev.x		= changes->x;
    ev.y		= changes->y;
    ev.width		= changes->width;
    ev.height		= changes->height;
    ev.border_width	= changes->border_width;
    ev.above		= changes->sibling;
    ev.detail		= changes->stack_mode;
    return (XSendEvent (dpy, root, False,
			SubstructureRedirectMask|SubstructureNotifyMask,
			(XEvent *)&ev));
}


static int restack_error_handler (dpy, ev)
    Display *dpy;
    XErrorEvent *ev;
{
    if (ev->serial == dpy->reconfigure_wm_window.sequence_number &&
	ev->error_code == BadMatch) {
	dpy->reconfigure_wm_window.succeeded = False;
	return 0;
    }
    return (*(dpy->reconfigure_wm_window.old_handler))(dpy, ev);
}
