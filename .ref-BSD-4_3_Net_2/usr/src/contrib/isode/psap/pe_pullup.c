/* pe_pullup.c - "pullup" a presentation element */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/psap/RCS/pe_pullup.c,v 7.1 91/02/22 09:36:16 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/psap/RCS/pe_pullup.c,v 7.1 91/02/22 09:36:16 mrose Interim $
 *
 *
 * $Log:	pe_pullup.c,v $
 * Revision 7.1  91/02/22  09:36:16  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  22:13:06  mrose
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


PElementData	pe_pullup_aux ();

/*  */

int	pe_pullup (pe)
register PE	pe;
{
    PElementLen	    len;
    register PElementData dp;
    register PE	    p;

    if (pe -> pe_form != PE_FORM_CONS)
	return OK;

    if ((dp = pe_pullup_aux (pe, &len)) == NULLPED)
	return NOTOK;

    for (p = pe -> pe_cons; p; p = p -> pe_next)
	pe_free (p);

    pe -> pe_form = PE_FORM_PRIM;
    pe -> pe_len = len;
    pe -> pe_prim = dp;

    return OK;
}

/*  */

static PElementData  pe_pullup_aux (pe, len)
register PE	pe;
register int   *len;
{
    register int    i,
                    k;
    int     j;
    register PElementClass class;
    register PElementID id;
    register PElementData dp,
			  ep,
			  fp;
    register PE	    p;

    switch (pe -> pe_form) {
	case PE_FORM_PRIM: 
	    if ((dp = PEDalloc (i = pe -> pe_len)) == NULLPED)
		return NULLPED;
	    PEDcpy (pe -> pe_prim, dp, i);
	    break;

	case PE_FORM_CONS: 
	    dp = NULLPED, i = 0;
	    class = pe -> pe_class, id = pe -> pe_id;
	    for (p = pe -> pe_cons; p; p = p -> pe_next) {
		if (p -> pe_class != class
			|| p -> pe_id != id
			|| (ep = pe_pullup_aux (p, &j)) == NULLPED) {
		    if (dp)
			PEDfree (dp);
		    return NULLPED;
		}
		if (dp) {
		    if ((fp = PEDrealloc (dp, k = i + j)) == NULLPED) {
			PEDfree (dp);
			return NULLPED;
		    }
		    PEDcpy (ep, fp + i, j);
		    dp = fp, i = k;
		}
		else
		    dp = ep, i += j;
	    }
	    break;

	case PE_FORM_ICONS:
	    return NULLPED;
    }

    *len = i;
    return (dp);
}
