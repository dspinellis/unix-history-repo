/* $XConsortium: XParseCol.c,v 11.27 91/07/22 22:34:30 rws Exp $ */
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

#if NeedFunctionPrototypes
Status XParseColor (
	register Display *dpy,
        Colormap cmap,
	_Xconst char *spec,
	XColor *def)
#else
Status XParseColor (dpy, cmap, spec, def)
	register Display *dpy;
        Colormap cmap;
	char *spec;
	XColor *def;
#endif
{
	register int n, i;
	int r, g, b;
	char c;
	XcmsCCC ccc;
	XcmsColor cmsColor;

        if (!spec) return(0);
	n = strlen (spec);
	if (*spec == '#') {
	    /*
	     * RGB
	     */
	    spec++;
	    n--;
	    if (n != 3 && n != 6 && n != 9 && n != 12)
		return (0);
	    n /= 3;
	    g = b = 0;
	    do {
		r = g;
		g = b;
		b = 0;
		for (i = n; --i >= 0; ) {
		    c = *spec++;
		    b <<= 4;
		    if (c >= '0' && c <= '9')
			b |= c - '0';
		    else if (c >= 'A' && c <= 'F')
			b |= c - ('A' - 10);
		    else if (c >= 'a' && c <= 'f')
			b |= c - ('a' - 10);
		    else return (0);
		}
	    } while (*spec != '\0');
	    n <<= 2;
	    n = 16 - n;
	    def->red = r << n;
	    def->green = g << n;
	    def->blue = b << n;
	    def->flags = DoRed | DoGreen | DoBlue;
	    return (1);
	}


	/*
	 * Let's Attempt to use TekCMS and i18n approach to Parse Color
	 */
	if ((ccc = XcmsCCCOfColormap(dpy, cmap)) != (XcmsCCC)NULL) {
	    if (_XcmsResolveColorString(ccc, &spec,
		    &cmsColor, XcmsRGBFormat) != XcmsFailure) {
		_XcmsRGB_to_XColor(&cmsColor, def, 1);
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
	{
	    xLookupColorReply reply;
	    register xLookupColorReq *req;
	    LockDisplay(dpy);
	    GetReq (LookupColor, req);
	    req->cmap = cmap;
	    req->nbytes = n = strlen(spec);
	    req->length += (n + 3) >> 2;
	    Data (dpy, spec, (long)n);
	    if (!_XReply (dpy, (xReply *) &reply, 0, xTrue)) {
		UnlockDisplay(dpy);
		SyncHandle();
		return (0);
		}
	    def->red = reply.exactRed;
	    def->green = reply.exactGreen;
	    def->blue = reply.exactBlue;
	    def->flags = DoRed | DoGreen | DoBlue;
	    UnlockDisplay(dpy);
	    SyncHandle();
	    return (1);
	}
}
