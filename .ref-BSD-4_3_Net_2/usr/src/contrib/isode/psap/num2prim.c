/* num2prim.c - integer to presentation element */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/psap/RCS/num2prim.c,v 7.1 91/02/22 09:35:49 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/psap/RCS/num2prim.c,v 7.1 91/02/22 09:35:49 mrose Interim $
 *
 *
 * $Log:	num2prim.c,v $
 * Revision 7.1  91/02/22  09:35:49  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  22:12:45  mrose
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

PE	num2prim (i, class, id)
register integer i;
PElementClass	class;
PElementID	id;
{
    register integer mask,
                    sign,
                    n;
    register PElementData dp;
    register PE	    pe;

    if ((pe = pe_alloc (class, PE_FORM_PRIM, id)) == NULLPE)
	return NULLPE;

    sign = i >= 0 ? i : i ^ (-1);
    mask = 0x1ff << (((n = sizeof i) - 1) * 8 - 1);
    while (n > 1 && (sign & mask) == 0)
	mask >>= 8, n--;

    if ((pe -> pe_prim = PEDalloc (n)) == NULLPED) {
	pe_free (pe);
	return NULLPE;
    }

    for (dp = pe -> pe_prim + (pe -> pe_len = n); n-- > 0; i >>= 8)
	*--dp = i & 0xff;

    return pe;
}
