/* $XConsortium: XcmsPrOfId.c,v 1.5 91/05/13 23:26:56 rws Exp $" */

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
 *		XcmsPrOfId.c
 *
 *	DESCRIPTION
 *		Source for XcmsPrefixOfFormat()
 *
 *
 */

#include "Xlibint.h"
#include "Xcmsint.h"

/*
 *      EXTERNS
 */
extern XcmsColorSpace **_XcmsDIColorSpaces;
extern XcmsColorSpace **_XcmsDDColorSpaces;


/*
 *	NAME
 *		XcmsPrefixOfId
 *
 *	SYNOPSIS
 */
char *
XcmsPrefixOfFormat(id)
    XcmsColorFormat	id;
/*
 *	DESCRIPTION
 *		Returns the color space prefix for the specified color
 *		space ID if the color space is found in the Color
 *		Conversion Context.
 *
 *	RETURNS
 *		Returns a color space prefix.
 *
 *	CAVEATS
 *		Space is allocated for the returned string, therefore,
 *		the application is responsible for freeing (using XFree)
 *		the space.
 *
 */
{
    XcmsColorSpace	**papColorSpaces;
    char *prefix;

    /*
     * First try Device-Independent color spaces
     */
    papColorSpaces = _XcmsDIColorSpaces;
    if (papColorSpaces != NULL) {
	while (*papColorSpaces != NULL) {
	    if ((*papColorSpaces)->id == id) {
		prefix = (char *)Xmalloc((strlen((*papColorSpaces)->prefix) +
		1) * sizeof(char));
		strcpy(prefix, (*papColorSpaces)->prefix);
		return(prefix);
	    }
	    papColorSpaces++;
	}
    }

    /*
     * Next try Device-Dependent color spaces
     */
    papColorSpaces = _XcmsDDColorSpaces;
    if (papColorSpaces != NULL) {
	while (*papColorSpaces != NULL) {
	    if ((*papColorSpaces)->id == id) {
		prefix = (char *)Xmalloc((strlen((*papColorSpaces)->prefix) +
		1) * sizeof(char));
		strcpy(prefix, (*papColorSpaces)->prefix);
		return(prefix);
	    }
	    papColorSpaces++;
	}
    }

    return(NULL);
}
