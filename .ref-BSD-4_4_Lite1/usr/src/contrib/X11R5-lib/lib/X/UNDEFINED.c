/* $XConsortium: UNDEFINED.c,v 1.5 91/07/25 01:10:56 rws Exp $" */

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
 *
 *	NAME
 *		UNDEFINED.c
 *
 *	DESCRIPTION
 *		UNDEFINED Color Space
 *
 *
 */

#include "Xlibint.h"
#include "Xcmsint.h"

/*
 *      FORWARD DECLARATIONS
 */
static int ReturnZero();


/*
 *      LOCALS VARIABLES
 */

static Status (*(Fl_ReturnZero[]))() = {
    ReturnZero,
    NULL
};




/*
 *      GLOBALS
 *              Variables declared in this package that are allowed
 *		to be used globally.
 */
    /*
     * UNDEFINED Color Space
     */
XcmsColorSpace	XcmsUNDEFINEDColorSpace =
    {
	"undefined",		/* prefix */
	XcmsUndefinedFormat,	/* id */
	ReturnZero,		/* parseString */
	Fl_ReturnZero,		/* to_CIEXYZ */
	Fl_ReturnZero		/* from_CIEXYZ */
    };



/************************************************************************
 *									*
 *			PRIVATE ROUTINES				*
 *									*
 ************************************************************************/

/*
 *	NAME
 *		ReturnZero
 *
 *	SYNOPSIS
 */
/* ARGSUSED */
static int
ReturnZero()
/*
 *	DESCRIPTION
 *		Does nothing.
 *
 *	RETURNS
 *		0
 *
 */
{
    return(0);
}
