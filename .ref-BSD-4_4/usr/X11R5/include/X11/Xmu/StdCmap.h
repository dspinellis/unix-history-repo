/* $XConsortium: StdCmap.h,v 1.3 91/07/22 23:46:03 converse Exp $
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

#ifndef _XMU_STDCMAP_H_
#define _XMU_STDCMAP_H_

#include <X11/Xfuncproto.h>

_XFUNCPROTOBEGIN

Status XmuAllStandardColormaps(
#if NeedFunctionPrototypes
    Display*		/* dpy */
#endif
);

Status XmuCreateColormap(
#if NeedFunctionPrototypes
    Display*		/* dpy */,
    XStandardColormap*	/* colormap */
#endif
);

void   XmuDeleteStandardColormap(
#if NeedFunctionPrototypes
    Display*		/* dpy */,
    int			/* screen */,
    Atom		/* property */
#endif
);

Status XmuGetColormapAllocation(
#if NeedFunctionPrototypes
    XVisualInfo*	/* vinfo */,
    Atom		/* property */,
    unsigned long*	/* red_max_return */,
    unsigned long*	/* green_max_return */,
    unsigned long*	/* blue_max_return */
#endif
);

Status XmuLookupStandardColormap(
#if NeedFunctionPrototypes
    Display*		/* dpy */,
    int			/* screen */,
    VisualID		/* visualid */,
    unsigned int	/* depth */,
    Atom		/* property */,
    Bool		/* replace */,
    Bool		/* retain */
#endif
);

XStandardColormap *XmuStandardColormap(
#if NeedFunctionPrototypes
    Display*		/* dpy */,
    int			/* screen */,
    VisualID		/* visualid */,
    unsigned int	/* depth */,
    Atom		/* property */,
    Colormap		/* cmap */,
    unsigned long	/* red_max */,
    unsigned long	/* green_max */,
    unsigned long	/* blue_max */
#endif
);

Status XmuVisualStandardColormaps(
#if NeedFunctionPrototypes
    Display*		/* dpy */,
    int			/* screen */,
    VisualID		/* visualid */,
    unsigned int	/* depth */,
    Bool		/* replace */,
    Bool		/* retain */
#endif
);

_XFUNCPROTOEND

#endif /* _XMU_STDCMAP_H_ */
