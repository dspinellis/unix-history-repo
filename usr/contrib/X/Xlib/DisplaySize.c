#include <X/mit-copyright.h>

/* $Header: DisplaySize.c,v 10.3 86/02/01 15:29:08 tony Rel $ */
/* Copyright	Massachusetts Institute of Technology 1985 */

#include "XlibInternal.h"

int DisplayWidth()
{
	register Display *dpy = _XlibCurrentDisplay;
	WindowInfo rootinfo;
	if (dpy->width == 0 || dpy->height == 0) {
		XQueryWindow(RootWindow, &rootinfo);
		dpy->width = rootinfo.width;
		dpy->height = rootinfo.height;
	}
	return (dpy->width);
}

int DisplayHeight()
{
	register Display *dpy = _XlibCurrentDisplay;
	WindowInfo rootinfo;
	if (dpy->width == 0 || dpy->height == 0) {
		XQueryWindow(RootWindow, &rootinfo);
		dpy->width = rootinfo.width;
		dpy->height = rootinfo.height;
	}
	return (dpy->height);
}
		
