#include <X/mit-copyright.h>

/* $Header: XFeep.c,v 10.4 86/02/01 15:33:18 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"

/*
 * XFeep - Ring the display's bell.  The sound volume is in the range -7
 * to 7 and is added to the base volume as defined by XFeepControl.  Large
 * numbers represent louder volumes.
 */

XFeep(volume)
	int volume;
{
	register Display *dpy = _XlibCurrentDisplay;
	register XReq *req;

	GetReq(X_Feep, dpy->root);
	req->param.s[0] = volume;
}

