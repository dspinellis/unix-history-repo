/************************************************************
Copyright 1989 by The Massachusetts Institute of Technology

Permission to use, copy, modify, and distribute this
software and its documentation for any purpose and without
fee is hereby granted, provided that the above copyright
no- tice appear in all copies and that both that copyright
no- tice and this permission notice appear in supporting
docu- mentation, and that the name of MIT not be used in
advertising or publicity pertaining to distribution of the
software without specific prior written permission.
M.I.T. makes no representation about the suitability of
this software for any purpose. It is provided "as is"
without any express or implied warranty.

********************************************************/

/* RANDOM CRUFT! THIS HAS NO OFFICIAL X CONSORTIUM BLESSING */

/* $XConsortium: MITMisc.h,v 1.4 91/07/12 10:06:43 rws Exp $ */

#ifndef _XMITMISC_H_
#define _XMITMISC_H_

#include <X11/Xfuncproto.h>

#define X_MITSetBugMode			0
#define X_MITGetBugMode			1

#define MITMiscNumberEvents		0

#define MITMiscNumberErrors		0

#ifndef _MITMISC_SERVER_

_XFUNCPROTOBEGIN

Bool XMITMiscQueryExtension(
#if NeedFunctionPrototypes
    Display*		/* dpy */,
    int*		/* event_basep */,
    int*		/* error_basep */
#endif
);

Status XMITMiscSetBugMode(
#if NeedFunctionPrototypes
    Display*		/* dpy */,
    Bool		/* onOff */
#endif
);

Bool XMITMiscGetBugMode(
#if NeedFunctionPrototypes
    Display*		/* dpy */
#endif
);

_XFUNCPROTOEND

#endif

#endif
