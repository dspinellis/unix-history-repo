#include <X/mit-copyright.h>

/* $Header: XFeep.c,v 10.5 86/04/22 15:28:06 jg Rel $ */
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
	req->params0 = volume;
}

