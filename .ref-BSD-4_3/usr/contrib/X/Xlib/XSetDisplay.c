#include <X/mit-copyright.h>

/* $Header: XSetDisplay.c,v 10.4 86/02/01 15:39:42 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"

/* 
 * XSetDisplay - This proceedure sets the current display (_XlibCurrentDisplay)
 * with which the library is communicating.
 */
 
XSetDisplay(dpy)
	register Display *dpy;
{
	_XlibCurrentDisplay = dpy;
}

