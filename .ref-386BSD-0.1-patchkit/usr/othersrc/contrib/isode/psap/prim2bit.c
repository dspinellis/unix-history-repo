/* prim2bit.c - presentation element to bit string */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/psap/RCS/prim2bit.c,v 7.1 91/02/22 09:36:20 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/psap/RCS/prim2bit.c,v 7.1 91/02/22 09:36:20 mrose Interim $
 *
 *
 * $Log:	prim2bit.c,v $
 * Revision 7.1  91/02/22  09:36:20  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  22:13:09  mrose
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

PE	prim2bit (pe)
register PE	pe;
{
    int	    i;
    register PElementData bp;
    register PElementLen len;
    register PE	    p;

    switch (pe -> pe_form) {
	case PE_FORM_PRIM:	/* very paranoid... */
	    if ((bp = pe -> pe_prim) && (len = pe -> pe_len)) {
		if ((i = *bp & 0xff) > 7)
		    return pe_seterr (pe, PE_ERR_BITS, NULLPE);
		pe -> pe_nbits = ((len - 1) * 8) - i;
	    }
	    else
		pe -> pe_nbits = 0;
	    break;

	case PE_FORM_CONS: 
	    pe -> pe_nbits = 0;
	    for (p = pe -> pe_cons; p; p = p -> pe_next) {
		if (prim2bit (p) == NULLPE)
		    return NULLPE;
		pe -> pe_nbits += p -> pe_nbits;
	    }
	    break;
    }

    return pe;
}
