#include <X/mit-copyright.h>

#include <X/Xlib.h>
#include <stdio.h>
/*
 * Copyright 1985, Massachusetts Institute of Technology.
 * This program just throws up a window over the whole screen, causing
 * exposure events to be generated on all windows.  This may be useful
 * to cause the whole screen to be repainted when it has somehow gotten
 * trashed.
 */
#ifndef lint
static char *rcsid_xrefresh_c = "$Header: xrefresh.c,v 10.5 86/02/01 16:16:40 tony Rel $";
#endif

main(argc, argv)
int argc;
char **argv;
{
	Window w;

	if (XOpenDisplay(argc ? argv[1] : "\0") == NULL) 
		fprintf (stderr, "Could not open Display!\n");

	w = XCreateWindow(RootWindow, 0, 0, DisplayWidth(), DisplayHeight(),
		0, (Pixmap) 0, (Pixmap) 0);
	XMapWindow(w);			/* put it up on the screen 	*/
	XDestroyWindow(w);		/* throw it away		*/

	XFlush();			/* and make sure the server sees it*/
}
	
