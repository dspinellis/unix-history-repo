/* pe_error.c - presentation element error to string */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/psap/RCS/pe_error.c,v 7.3 91/02/22 09:36:12 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/psap/RCS/pe_error.c,v 7.3 91/02/22 09:36:12 mrose Interim $
 *
 *
 * $Log:	pe_error.c,v $
 * Revision 7.3  91/02/22  09:36:12  mrose
 * Interim 6.8
 * 
 * Revision 7.2  91/01/11  07:09:08  mrose
 * jpo
 * 
 * Revision 7.1  90/10/23  20:43:44  mrose
 * update
 * 
 * Revision 7.0  89/11/23  22:13:03  mrose
 * Release 6.0
 * 
 */

/*
 *				  NOTICE
 *
 *    Acquisition, use, and distribution of this module and related
 *    materials are subject to the restrictions of a license agreement.
 *    Consult the Preface in the User's Manual for the full terms of
 *    this agreement.
 *
 */


/* LINTLIBRARY */

#include <stdio.h>
#include "psap.h"

/*  */

/*  */

static char *pe_errorlist[] = {
    "Error 0",
    "Overflow",
    "Out of memory",
    "No such bit",
    "Malformed universal timestring",
    "Malformed generalized timestring",
    "No such member",
    "Not a primitive form",
    "Not a constructor form",
    "Class/ID mismatch in constructor",
    "Malformed object identifier",
    "Malformed bitstring",
    "Type not supported",
    "Signed integer not expected"
};

static int pe_maxerror = sizeof pe_errorlist / sizeof pe_errorlist[0];

/*  */

char   *pe_error (c)
int	c;
{
    register char  *bp;
    static char buffer[30];

    if (c < pe_maxerror && (bp = pe_errorlist[c]))
	return bp;

    (void) sprintf (buffer, "Error %d", c);
    return buffer;
}
