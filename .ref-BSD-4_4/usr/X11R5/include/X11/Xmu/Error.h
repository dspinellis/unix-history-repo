/* $XConsortium: Error.h,v 1.4 91/07/22 23:45:54 converse Exp $
 *
 * Copyright 1988 by the Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided 
 * that the above copyright notice appear in all copies and that both that 
 * copyright notice and this permission notice appear in supporting 
 * documentation, and that the name of M.I.T. not be used in advertising
 * or publicity pertaining to distribution of the software without specific, 
 * written prior permission. M.I.T. makes no representations about the 
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * The X Window System is a Trademark of MIT.
 *
 * The interfaces described by this header file are for miscellaneous utilities
 * and are not part of the Xlib standard.
 */

#ifndef _XMU_ERROR_H_
#define _XMU_ERROR_H_

#include <X11/Xfuncproto.h>

_XFUNCPROTOBEGIN

extern int XmuPrintDefaultErrorMessage(
#if NeedFunctionPrototypes
    Display*		/* dpy */,
    XErrorEvent*	/* event */,
    FILE*		/* fp */
#endif
);

extern int XmuSimpleErrorHandler(
#if NeedFunctionPrototypes
    Display*		/* dpy */,
    XErrorEvent*	/* errorp */
#endif
);

_XFUNCPROTOEND

#endif /* _XMU_ERROR_H_ */
