/* converts a bit string - output of bitstr2strb() - to an integer */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/psap/RCS/strb2int.c,v 7.1 91/02/22 09:37:12 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/psap/RCS/strb2int.c,v 7.1 91/02/22 09:37:12 mrose Interim $
 *
 *
 * $Log:	strb2int.c,v $
 * Revision 7.1  91/02/22  09:37:12  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  22:13:51  mrose
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


int	strb2int (cp, len)
register char   *cp;
register int	len;
{
    register int    i,
		    j,
		    bit,
		    mask,
		    n;

    n = 0;
    for (bit = (*cp & 0xff), i = 0, mask = 1 << (j = 7); i < len; i++) {
	if (bit & mask)
	    n |= 1 << i;
	if (j-- == 0)
	    bit = *++cp & 0xff, mask = 1 << (j = 7);
	else
	    mask >>= 1;
    }

    return n;
}
