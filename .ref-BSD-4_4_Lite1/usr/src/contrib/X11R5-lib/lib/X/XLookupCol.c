/* $XConsortium: XLookupCol.c,v 11.19 91/07/22 22:34:03 rws Exp $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

/*
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

#define NEED_REPLIES
#include <stdio.h>
#include "Xlibint.h"
#include "Xcmsint.h"

extern void _XcmsRGB_to_XColor();
extern void _XUnresolveColor();

#if NeedFunctionPrototypes
Status XLookupColor (
	register Display *dpy,
        Colormap cmap,
	_Xconst char *spec,
	XColor *def,
	XColor *scr)
#else
Status XLookupColor (dpy, cmap, spec, def, scr)
	register Display *dpy;
        Colormap cmap;
	char *spec;
	XColor *def, *scr;
#endif
{
	register int n;
	xLookupColorReply reply;
	register xLookupColorReq *req;
	XcmsCCC ccc;
	XcmsColor cmsColor_exact;

	/*
	 * Let's Attempt to use TekCMS and i18n approach to Parse Color
	 */
	/* copy string to allow overwrite by _XcmsResolveColorString() */
	if ((ccc = XcmsCCCOfColormap(dpy, cmap)) != (XcmsCCC)NULL) {
	    if (_XcmsResolveColorString(ccc, &spec,
		    &cmsColor_exact, XcmsRGBFormat) != XcmsFailure) {
		_XcmsRGB_to_XColor(&cmsColor_exact, def, 1);
		bcopy((char *)def, (char *)scr, sizeof(XColor));
		_XUnresolveColor(ccc, scr);
		return(1);
	    }
	    /*
	     * Otherwise we failed; or spec was changed with yet another
	     * name.  Thus pass name to the X Server.
	     */
	}


	/*
	 * TekCMS and i18n methods failed, so lets pass it to the server
	 * for parsing.
	 */

	n = strlen (spec);
	LockDisplay(dpy);
	GetReq (LookupColor, req);
	req->cmap = cmap;
	req->nbytes = n;
	req->length += (n + 3) >> 2;
	Data (dpy, spec, (long)n);
	if (!_XReply (dpy, (xReply *) &reply, 0, xTrue)) {
	    UnlockDisplay(dpy);
	    SyncHandle();
	    return (0);
	    }
	def->red   = reply.exactRed;
	def->green = reply.exactGreen;
	def->blue  = reply.exactBlue;

	scr->red   = reply.screenRed;
	scr->green = reply.screenGreen;
	scr->blue  = reply.screenBlue;

	UnlockDisplay(dpy);
	SyncHandle();
	return (1);
}
