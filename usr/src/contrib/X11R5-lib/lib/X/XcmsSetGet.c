/* $XConsortium: XcmsSetGet.c,v 1.5 91/05/13 23:28:55 rws Exp $" */

/*
 * Code and supporting documentation (c) Copyright 1990 1991 Tektronix, Inc.
 * 	All Rights Reserved
 * 
 * This file is a component of an X Window System-specific implementation
 * of Xcms based on the TekColor Color Management System.  Permission is
 * hereby granted to use, copy, modify, sell, and otherwise distribute this
 * software and its documentation for any purpose and without fee, provided
 * that this copyright, permission, and disclaimer notice is reproduced in
 * all copies of this software and in supporting documentation.  TekColor
 * is a trademark of Tektronix, Inc.
 * 
 * Tektronix makes no representation about the suitability of this software
 * for any purpose.  It is provided "as is" and with all faults.
 * 
 * TEKTRONIX DISCLAIMS ALL WARRANTIES APPLICABLE TO THIS SOFTWARE,
 * INCLUDING THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE.  IN NO EVENT SHALL TEKTRONIX BE LIABLE FOR ANY
 * SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER
 * RESULTING FROM LOSS OF USE, DATA, OR PROFITS, WHETHER IN AN ACTION OF
 * CONTRACT, NEGLIGENCE, OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR THE PERFORMANCE OF THIS SOFTWARE.
 *
 *
 *	NAME
 *		XcmsSetGet.c
 *
 *	DESCRIPTION
 *		Source for _XcmsSetGetColors()
 *
 *
 */

/*
 *      EXTERNAL INCLUDES
 *              Include files that must be exported to any package or
 *              program using this package.
 */
#include "Xlibint.h"
#include "Xcmsint.h"


/*
 *      EXTERNS
 */

extern void _XcmsRGB_to_XColor();
extern void _XColor_to_XcmsRGB();



/************************************************************************
 *									*
 *			API PRIVATE ROUTINES				*
 *									*
 ************************************************************************/

/*
 *	NAME
 *		XcmsSetColors - 
 *
 *	SYNOPSIS
 */
Status
_XcmsSetGetColors(xColorProc, dpy, cmap, pColors_in_out, nColors,
	result_format, pCompressed)
    Status (*xColorProc)();
    Display *dpy;
    Colormap cmap;
    XcmsColor *pColors_in_out;
    unsigned int nColors;
    XcmsColorFormat result_format;
    Bool *pCompressed;
/*
 *	DESCRIPTION
 *		Routine containing code common to:
 *			XcmsAllocColor
 *			XcmsQueryColor
 *			XcmsQueryColors
 *			XcmsStoreColor
 *			XcmsStoreColors
 *
 *	RETURNS
 *		XcmsFailure if failed;
 *		XcmsSuccess if it succeeded without gamut compression;
 *		XcmsSuccessWithCompression if it succeeded with gamut
 *			compression;
 */
{
    XcmsCCC ccc;
    XColor *pXColors_in_out;
    Status retval = XcmsSuccess;

    /*
     * Argument Checking
     *	1. Assume xColorProc is correct
     *	2. Insure ccc not NULL
     *	3. Assume cmap correct (should be checked by Server)
     *	4. Insure pColors_in_out valid
     *	5. Assume method_in is valid (should be checked by Server)
     *	6. Insure nColors > 0
     */

    if (dpy == NULL) {
	return(XcmsFailure);
    }

    if (nColors == 0) {
	return(XcmsSuccess);
    }

    if (result_format == XcmsUndefinedFormat) {
	return(XcmsFailure);
    }

    if (!((*xColorProc == XAllocColor) || (*xColorProc == XStoreColor)
	    || (*xColorProc == XStoreColors) || (*xColorProc == XQueryColor) 
	    || (*xColorProc == XQueryColors))) {
	return(XcmsFailure);
    }

    if ((ccc = XcmsCCCOfColormap(dpy, cmap)) == (XcmsCCC)NULL) {
	return(XcmsFailure);
    }

    if ((*xColorProc == XAllocColor) || (*xColorProc == XStoreColor)
	    || (*xColorProc == XQueryColor)) {
	nColors = 1;
    }

    /*
     * Allocate space for XColors
     */
    if ((pXColors_in_out = (XColor *)Xcalloc(nColors, sizeof(XColor))) ==
	    NULL) {
	return(XcmsFailure);
    }

    if ((*xColorProc == XQueryColor) || (*xColorProc == XQueryColors)) {
	goto Query;
    }
    /*
     * Convert to RGB, adjusting for white point differences if necessary.
     */
    if ((retval = XcmsConvertColors(ccc, pColors_in_out, nColors, XcmsRGBFormat,
	    pCompressed)) == XcmsFailure) {
	return(XcmsFailure);
    }

Query:
    /*
     * Convert XcmsColor to XColor structures
     */
    _XcmsRGB_to_XColor(pColors_in_out, pXColors_in_out, nColors);

    /*
     * Now make appropriate X Call
     */
    if (*xColorProc == XAllocColor) {
	if ((*xColorProc)(ccc->dpy, cmap, pXColors_in_out) == 0) {
	    Xfree((char *)pXColors_in_out);
	    return(XcmsFailure);
	}
    } else if ((*xColorProc == XQueryColor) || (*xColorProc == XStoreColor)) {
	/* Note: XQueryColor and XStoreColor do not return any Status */
	(*xColorProc)(ccc->dpy, cmap, pXColors_in_out);
    } else if ((*xColorProc == XQueryColors) || (*xColorProc == XStoreColors)){
	/* Note: XQueryColors and XStoreColors do not return any Status */
	(*xColorProc)(ccc->dpy, cmap, pXColors_in_out, nColors);
    } else {
	Xfree((char *)pXColors_in_out);
	return(XcmsFailure);
    }

    if ((*xColorProc == XStoreColor) || (*xColorProc == XStoreColors)) {
	Xfree((char *)pXColors_in_out);
	return(retval);
    }

    /*
     * Now, convert returned XColor(i.e., rgb) to XcmsColor structures
     */
    _XColor_to_XcmsRGB(ccc, pXColors_in_out, pColors_in_out, nColors);
    Xfree((char *)pXColors_in_out);

    /*
     * Then, convert XcmsColor structures to the original specification
     *    format.  Note that we must use NULL instead of passing
     *    pCompressed.
     */

    if (result_format != XcmsRGBFormat) {
	if (XcmsConvertColors(ccc, pColors_in_out, nColors, result_format,
		(Bool *) NULL) == XcmsFailure) {
	    return(XcmsFailure);
	}
    }
    return(retval);
}
