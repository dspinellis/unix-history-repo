/* $XConsortium: SetWMProps.c,v 1.7 91/04/01 20:18:31 gildea Exp $ */

/***********************************************************
Copyright 1988 by Wyse Technology, Inc., San Jose, Ca.,
and the Massachusetts Institute of Technology, Cambridge, Massachusetts.
Copyright 1987 by Digital Equipment Corporation, Maynard, Massachusetts,
and the Massachusetts Institute of Technology, Cambridge, Massachusetts.

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the names of Wyse or MIT not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.  

WYSE DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************/

#include <X11/Xlibint.h>
#include <X11/Xatom.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>

#ifdef X_NOT_STDC_ENV
extern char *getenv();
#endif

/* 
 * XSetWMProperties sets the following properties:
 *	WM_NAME		  type: TEXT		format: varies?
 *	WM_ICON_NAME	  type: TEXT		format: varies?
 *	WM_HINTS	  type: WM_HINTS	format: 32
 *	WM_COMMAND	  type: TEXT		format: varies?
 *	WM_CLIENT_MACHINE type: TEXT		format: varies?
 *	WM_NORMAL_HINTS	  type: WM_SIZE_HINTS 	format: 32
 *	WM_CLASS	  type: STRING/STRING	format: 8
 */
	
void XSetWMProperties (dpy, w, windowName, iconName, argv, argc, sizeHints,
		       wmHints, classHints)
     Display *dpy;
     Window w;			/* window to decorate */
     XTextProperty *windowName;	/* name of application */
     XTextProperty *iconName;	/* name string for icon */
     char **argv;		/* command line */
     int argc;			/* size of command line */
     XSizeHints *sizeHints;	/* size hints for window in its normal state */
     XWMHints *wmHints;		/* miscelaneous window manager hints */
     XClassHint *classHints;	/* resource name and class */
{
    XTextProperty textprop;
    char hostName[256];
    int len = _XGetHostname (hostName, sizeof hostName);

    /* set names of window and icon */
    if (windowName) XSetWMName (dpy, w, windowName);
    if (iconName) XSetWMIconName (dpy, w, iconName);

    /* set the command if given */
    if (argv) {
	/*
	 * for UNIX and other operating systems which use nul-terminated
	 * arrays of STRINGs.
	 */
	XSetCommand (dpy, w, argv, argc);
    }

    /* set the name of the machine on which this application is running */
    textprop.value = (unsigned char *) hostName;
    textprop.encoding = XA_STRING;
    textprop.format = 8;
    textprop.nitems = len;
    XSetWMClientMachine (dpy, w, &textprop);
	
    /* set hints about how geometry and window manager interaction */
    if (sizeHints) XSetWMNormalHints (dpy, w, sizeHints);
    if (wmHints) XSetWMHints (dpy, w, wmHints);
    if (classHints) {
	XClassHint tmp;

	if (!classHints->res_name) {
	    tmp.res_name = getenv ("RESOURCE_NAME");
	    if (!tmp.res_name && argv && argv[0]) {
		/*
		 * UNIX uses /dir/subdir/.../basename; other operating
		 * systems will have to change this.
		 */
		char *cp = rindex (argv[0], '/');
		tmp.res_name = (cp ? cp + 1 : argv[0]);
	    }
	    tmp.res_class = classHints->res_class;
	    classHints = &tmp;
	}
	XSetClassHint (dpy, w, classHints);
    }
}

