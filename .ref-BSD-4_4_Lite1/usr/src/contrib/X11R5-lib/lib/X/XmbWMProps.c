/* $XConsortium: XmbWMProps.c,v 1.4 91/06/19 19:38:43 rws Exp $ */

/*

Copyright 1991 by the Massachusetts Institute of Technology

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

#include <X11/Xlibint.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>
#include <X11/Xlocale.h>

#if NeedFunctionPrototypes
void XmbSetWMProperties (
    Display *dpy,
    Window w,
    _Xconst char *windowName,
    _Xconst char *iconName,
    char **argv,
    int argc,
    XSizeHints *sizeHints,
    XWMHints *wmHints,
    XClassHint *classHints)
#else
void XmbSetWMProperties (dpy, w, windowName, iconName, argv, argc, sizeHints,
			 wmHints, classHints)
     Display *dpy;
     Window w;			/* window to decorate */
     char *windowName;		/* name of application */
     char *iconName;		/* name string for icon */
     char **argv;		/* command line */
     int argc;			/* size of command line */
     XSizeHints *sizeHints;	/* size hints for window in its normal state */
     XWMHints *wmHints;		/* miscelaneous window manager hints */
     XClassHint *classHints;	/* resource name and class */
#endif
{
    XTextProperty wname, iname;
    XTextProperty *wprop = NULL;
    XTextProperty *iprop = NULL;
    char *locale;

    if (windowName &&
	XmbTextListToTextProperty(dpy, (char**)&windowName, 1,
				   XStdICCTextStyle, &wname) >= Success)
	wprop = &wname;
    if (iconName &&
	XmbTextListToTextProperty(dpy, (char**)&iconName, 1,
				   XStdICCTextStyle, &iname) >= Success)
	iprop = &iname;
    XSetWMProperties(dpy, w, wprop, iprop, argv, argc,
		     sizeHints, wmHints, classHints);
    if (wprop)
	Xfree((char *)wname.value);
    if (iprop)
	Xfree((char *)iname.value);
    locale = setlocale(LC_CTYPE, (char *)NULL);
    if (locale)
	XChangeProperty (dpy, w, XInternAtom(dpy, "WM_LOCALE_NAME", False),
			 XA_STRING, 8, PropModeReplace,
			 (unsigned char *)locale, strlen(locale));
}
