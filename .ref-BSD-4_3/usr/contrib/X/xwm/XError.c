#include <X/mit-copyright.h>

/* Copyright    Massachusetts Institute of Technology    1985	*/

/*
 * 	XError - non-fatal error reporting routine.  Called when an
 * 	X_Error packet is encountered in the input stream.
 *
 *	File:		XError.c
 */

#ifndef lint
static char *rcsid_XError_c = "$Header: XError.c,v 10.3 86/02/01 16:10:25 tony Rel $";
#endif

#include "xwm.h"

XError(dpy, rep)
    Display *dpy;
    XErrorEvent *rep;
{
    if (!Debug) return;
    fprintf(stderr, "\n");
    fprintf(stderr, "xwm: X error occured during an xwm operation.\n");
    fprintf(stderr, "     Description: '%s'\n", XErrDescrip(rep->error_code));
    fprintf(stderr, "     Request code: %d\n", rep->request_code);
    fprintf(stderr, "     Request function: %d\n", rep->func);
    fprintf(stderr, "     Request window 0x%x\n", rep->window);
    fprintf(stderr, "     Error Serial #%d\n", rep->serial);
    fprintf(stderr, "     Current serial #%d\n", dpy->request);
    fprintf(stderr, "\n");
}
