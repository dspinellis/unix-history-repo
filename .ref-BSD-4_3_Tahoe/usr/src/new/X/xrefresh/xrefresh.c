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
static char *rcsid_xrefresh_c = "$Header: xrefresh.c,v 10.7 86/11/19 19:47:57 jg Rel $";
#endif

main(argc, argv)
int argc;
char **argv;
{
	Window w;

	if (XOpenDisplay(argc ? argv[1] : "\0") == NULL) {
	    fprintf(stderr, "%s: Can't open display '%s'\n",
		    argv[0], XDisplayName(argc ? argv[1] : "\0"));
	    exit(1);
	}


	w = XCreateWindow(RootWindow, 0, 0, DisplayWidth(), DisplayHeight(),
		0, (Pixmap) 0, (Pixmap) 0);
	XMapWindow(w);			/* put it up on the screen 	*/
	XDestroyWindow(w);		/* throw it away		*/

	XFlush();			/* and make sure the server sees it*/
}
	
