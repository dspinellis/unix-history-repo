/* $XConsortium: CIExyY.c,v 1.7 91/07/25 01:07:54 rws Exp $" */

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
 *	NAME
 *		CIExyY.c
 *
 *	DESCRIPTION
 *		This file contains routines that support the CIE xyY
 *		color space to include conversions to and from the CIE
 *		XYZ space.
 *
 *	DOCUMENTATION
 *		"TekColor Color Management System, System Implementor's Manual"
 */

#include <stdio.h>
#include <X11/Xos.h>
#include "Xlibint.h"
#include "Xcmsint.h"

/*
 *	DEFINES
 */
#define EPS 0.00001	/* some extremely small number */
#ifdef DBL_EPSILON
#  define XMY_DBL_EPSILON DBL_EPSILON
#else
#  define XMY_DBL_EPSILON 0.00001
#endif

/*
 *	EXTERNS
 */

extern char XcmsCIExyY_prefix[];

/*
 *	FORWARD DECLARATIONS
 */

static int CIExyY_ParseString();
Status XcmsCIExyY_ValidSpec();


/*
 *	LOCAL VARIABLES
 */

    /*
     * NULL terminated list of functions applied to get from CIExyY to CIEXYZ
     */
static XcmsConversionProc Fl_CIExyY_to_CIEXYZ[] = {
    XcmsCIExyYToCIEXYZ,
    NULL
};

    /*
     * NULL terminated list of functions applied to get from CIEXYZ to CIExyY
     */
static XcmsConversionProc Fl_CIEXYZ_to_CIExyY[] = {
    XcmsCIEXYZToCIExyY,
    NULL
};


/*
 *	GLOBALS
 */

    /*
     * CIE xyY Color Space
     */
XcmsColorSpace	XcmsCIExyYColorSpace =
    {
	XcmsCIExyY_prefix,	/* prefix */
	XcmsCIExyYFormat,		/* id */
	CIExyY_ParseString,	/* parseString */
	Fl_CIExyY_to_CIEXYZ,	/* to_CIEXYZ */
	Fl_CIEXYZ_to_CIExyY,	/* from_CIEXYZ */
	1
    };



/************************************************************************
 *									*
 *			 PRIVATE ROUTINES				*
 *									*
 ************************************************************************/

/*
 *	NAME
 *		CIExyY_ParseString
 *
 *	SYNOPSIS
 */
static int
CIExyY_ParseString(spec, pColor)
    register char *spec;
    XcmsColor *pColor;
/*
 *	DESCRIPTION
 *		This routines takes a string and attempts to convert
 *		it into a XcmsColor structure with XcmsCIExyYFormat.
 *		The assumed CIExyY string syntax is:
 *		    CIExyY:<x>/<y>/<Y>
 *		Where x, y, and Y are in string input format for floats
 *		consisting of:
 *		    a. an optional sign
 *		    b. a string of numbers possibly containing a decimal point,
 *		    c. an optional exponent field containing an 'E' or 'e'
 *			followed by a possibly signed integer string.
 *
 *	RETURNS
 *		0 if failed, non-zero otherwise.
 */
{
    int n;
    char *pchar;

    if ((pchar = strchr(spec, ':')) == NULL) {
	return(XcmsFailure);
    }
    n = (int)(pchar - spec);

    /*
     * Check for proper prefix.
     */
    if (strncmp(spec, XcmsCIExyY_prefix, n) != 0) {
	return(XcmsFailure);
    }

    /*
     * Attempt to parse the value portion.
     */
    if (sscanf(spec + n + 1, "%lf/%lf/%lf",
	    &pColor->spec.CIExyY.x,
	    &pColor->spec.CIExyY.y,
	    &pColor->spec.CIExyY.Y) != 3) {
	return(XcmsFailure);
    }
    pColor->format = XcmsCIExyYFormat;
    pColor->pixel = 0;
    return(XcmsCIExyY_ValidSpec(pColor));
}



/************************************************************************
 *									*
 *			 PUBLIC ROUTINES				*
 *									*
 ************************************************************************/

/*
 *	NAME
 *		CIExyY_ValidSpec()
 *
 *	SYNOPSIS
 */
Status
XcmsCIExyY_ValidSpec(pColor)
    XcmsColor *pColor;
/*
 *	DESCRIPTION
 *		Checks a valid CIExyY color specification.
 *
 *	RETURNS
 *		XcmsFailure if invalid.
 *		XcmsSuccess if valid.
 *
 */
{
    if (pColor->format != XcmsCIExyYFormat
	    ||
	    (pColor->spec.CIExyY.x < 0.0 - XMY_DBL_EPSILON)
	    ||
	    (pColor->spec.CIExyY.x > 1.0 + XMY_DBL_EPSILON)
	    ||
	    (pColor->spec.CIExyY.y < 0.0 - XMY_DBL_EPSILON)
	    ||
	    (pColor->spec.CIExyY.y > 1.0 + XMY_DBL_EPSILON)
	    ||
	    (pColor->spec.CIExyY.Y < 0.0 - XMY_DBL_EPSILON)
	    ||
	    (pColor->spec.CIExyY.Y > 1.0 + XMY_DBL_EPSILON)) {
	return(XcmsFailure);
    }
    return(XcmsSuccess);
}


/*
 *	NAME
 *		XcmsCIExyYToCIEXYZ - convert CIExyY to CIEXYZ
 *
 *	SYNOPSIS
 */
Status
XcmsCIExyYToCIEXYZ(ccc, pxyY_WhitePt, pColors_in_out, nColors)
    XcmsCCC ccc;
    XcmsColor *pxyY_WhitePt;
    XcmsColor *pColors_in_out;
    unsigned int nColors;
/*
 *	DESCRIPTION
 *		Converts color specifications in an array of XcmsColor
 *		structures from CIExyY format to CIEXYZ format.
 *
 *	RETURNS
 *		XcmsFailure if failed,
 *		XcmsSuccess if succeeded.
 */
{
    XcmsColor	*pColor = pColors_in_out;
    XcmsColor	whitePt;
    XcmsCIEXYZ	XYZ_return;
    XcmsFloat	div;		/* temporary storage in case divisor is zero */
    XcmsFloat	u, v, x, y, z;	/* temporary storage */
    register int i;

    /*
     * Check arguments
     */
    if (pxyY_WhitePt == NULL || pColors_in_out == NULL) {
	return(XcmsFailure);
    }


    /*
     * Now convert each XcmsColor structure to CIEXYZ form
     */
    for (i = 0; i < nColors; i++, pColor++) {
	/* Make sure original format is CIExyY and valid */
	if (!XcmsCIExyY_ValidSpec(pColor)) {
	    return(XcmsFailure);
	}

	if ((div = (-2 * pColor->spec.CIExyY.x) + (12 * pColor->spec.CIExyY.y) + 3) == 0.0) {
	    /* Note that the divisor is zero */
	    /* This return is abitrary. */
	    XYZ_return.X = 0;
	    XYZ_return.Y = 0;
	    XYZ_return.Z = 0;
	} else {
	    /*
	     * Make sure white point is in CIEXYZ form
	     */
	    if (pxyY_WhitePt->format != XcmsCIEXYZFormat) {
		/* Make copy of the white point because we're going to modify it */
		bcopy((char *)pxyY_WhitePt, (char *)&whitePt, sizeof(XcmsColor));
		if (!_XcmsDIConvertColors(ccc, &whitePt, (XcmsColor *)NULL, 1,
			XcmsCIEXYZFormat)) {
		    return(XcmsFailure);
		}
		pxyY_WhitePt = &whitePt;
	    }

	    /* Make sure it is a white point, i.e., Y == 1.0 */
	    if (pxyY_WhitePt->spec.CIEXYZ.Y != 1.0) {
		return(XcmsFailure);
	    }

	    /* Convert from xyY to uvY to XYZ */
	    u = (4 * pColor->spec.CIExyY.x) / div;
	    v = (9 * pColor->spec.CIExyY.y) / div;
	    div = (6.0 * u) - (16.0 * v) + 12.0;
	    if (div == 0.0) {
		/* Note that the divisor is zero */
		/* This return is abitrary. */
		if ((div = (6.0 * whitePt.spec.CIEuvY.u_prime) -
		           (16.0 * whitePt.spec.CIEuvY.v_prime) + 12.0) == 0.0) {
		    div = EPS;
		}
		x = 9.0 * whitePt.spec.CIEuvY.u_prime / div;
		y = 4.0 * whitePt.spec.CIEuvY.u_prime / div;
	    } else {
		/* convert u, v to small xyz */
		x = 9.0 * u / div;
		y = 4.0 * v / div;
	    }
	    z = 1.0 - x - y;
	    if (y == 0.0) y = EPS;	/* Have to worry about divide by 0 */
	    XYZ_return.Y = pColor->spec.CIExyY.Y;
	    XYZ_return.X = x * XYZ_return.Y / y;
	    XYZ_return.Z = z * XYZ_return.Y / y;
	}

	/* Copy result to pColor */
	bcopy ((char *)&XYZ_return, (char *)&pColor->spec, sizeof(XcmsCIEXYZ));

	/* Identify that the format is now CIEXYZ */
	pColor->format = XcmsCIEXYZFormat;
    }
    return(XcmsSuccess);
}


/*
 *	NAME
 *		XcmsCIEXYZToCIExyY - convert CIEXYZ to CIExyY
 *
 *	SYNOPSIS
 */
/* ARGSUSED */
Status
XcmsCIEXYZToCIExyY(ccc, pxyY_WhitePt, pColors_in_out, nColors)
    XcmsCCC ccc;
    XcmsColor *pxyY_WhitePt;
    XcmsColor *pColors_in_out;
    unsigned int nColors;
/*
 *	DESCRIPTION
 *		Converts color specifications in an array of XcmsColor
 *		structures from CIEXYZ format to CIExyY format.
 *
 *	RETURNS
 *		XcmsFailure if failed,
 *		XcmsSuccess if succeeded.
 *
 */
{
    XcmsColor	*pColor = pColors_in_out;
    XcmsCIExyY	xyY_return;
    XcmsFloat	div;		/* temporary storage in case divisor is zero */
    register int i;

    /*
     * Check arguments
     * 		pxyY_WhitePt ignored
     */
    if (pColors_in_out == NULL) {
	return(XcmsFailure);
    }

    /*
     * Now convert each XcmsColor structure to CIEXYZ form
     */
    for (i = 0; i < nColors; i++, pColor++) {

	if (!XcmsCIEXYZ_ValidSpec(pColor)) {
	    return(XcmsFailure);
	}
	/* Now convert for XYZ to xyY */
	if ((div = pColor->spec.CIEXYZ.X + pColor->spec.CIEXYZ.Y + pColor->spec.CIEXYZ.Z) == 0.0) {
	    div = EPS;
	} 
	xyY_return.x = pColor->spec.CIEXYZ.X / div;
	xyY_return.y = pColor->spec.CIEXYZ.Y / div;
	xyY_return.Y = pColor->spec.CIEXYZ.Y;

	/* Copy result to pColor */
	bcopy ((char *)&xyY_return, (char *)&pColor->spec, sizeof(XcmsCIExyY));

	/* Identify that the format is now CIEXYZ */
	pColor->format = XcmsCIExyYFormat;
    }
    return(XcmsSuccess);
}
