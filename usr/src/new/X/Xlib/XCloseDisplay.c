#include <X/mit-copyright.h>

/* $Header: XCloseDisplay.c,v 10.5 86/04/22 15:19:04 jg Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"

/* 
 * XCloseDisplay - XSync the connection to the X Server, close the connection,
 * and free all associated storage.  Signals must be masked out during this
 * operation to guarantee atomicity.
 */
XCloseDisplay (dpy)
	register Display *dpy;
{
	register int sig_mask;
	
	/* 
	 * Mask out all signals so that the library internal state can
	 * be sync'ed with the rest of the world.  We don't want to be
	 * interupted while we are flushing and freeing buffers.
	 */
	sig_mask = sigsetmask(-1);
	XSync(TRUE);
	if (close(dpy->fd) == -1) {
		/* Argh! someone already closed the descriptor! */
		_XIOError(_XlibCurrentDisplay);
	}
	sigsetmask(sig_mask);		/* Return signals to normal. */
	if (dpy->displayname) free(dpy->displayname);
	free(dpy->buffer);
	free((char *)dpy);
}
