/* pe_free.c - free a presentation element */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/psap/RCS/pe_free.c,v 7.4 91/03/09 11:55:31 mrose Exp $";
#endif

/* 
 * $Header: /f/osi/psap/RCS/pe_free.c,v 7.4 91/03/09 11:55:31 mrose Exp $
 *
 *
 * $Log:	pe_free.c,v $
 * Revision 7.4  91/03/09  11:55:31  mrose
 * update
 * 
 * Revision 7.3  91/02/22  09:36:15  mrose
 * Interim 6.8
 * 
 * Revision 7.2  91/02/20  17:26:42  mrose
 * update
 * 
 * Revision 7.1  91/01/24  14:50:26  mrose
 * update
 * 
 * Revision 7.0  89/11/23  22:13:05  mrose
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
#include "tailor.h"

/*  */

int	pe_free (pe)
register PE	pe;
{
    register PE	    p,
		    q;
    register struct qbuf *qb,
                         *qp;

    if (!pe)
        abort ();

    if( pe->pe_refcnt < 0) {
      DLOG (psap_log, LLOG_DEBUG,
	    ("WARNING: duplicately free'd pe 0x%x!", pe));
      return;
    }

    if (pe -> pe_refcnt-- > 0)
	return;

    switch (pe -> pe_form) {
	case PE_FORM_PRIM: 
	case PE_FORM_ICONS: 
	    if (pe -> pe_prim && !pe -> pe_inline)
		PEDfree (pe -> pe_prim);
            else
		if (pe -> pe_inline && Hqb) {
		    if(Fqb && (--Qbrefs == 0)) {
			for (qb = Fqb; qb && (qb != Hqb); qb = qp) {
			    qp = qb -> qb_forw;
			    free ((char *) qb);
			}
			if (!qb)
			    abort ();
			Fqb = Hqb = NULL;
		    }
		}
	    break;

	case PE_FORM_CONS: 
	    for (p = pe -> pe_cons; p; p = q) {
		q = p -> pe_next;
		pe_free (p);
	    }
	    break;

        default:
            abort();
	    /* NOTREACHED */
    }

    if (pe -> pe_realbase)
	free (pe -> pe_realbase);
	
    pe_frees++;
    pe -> pe_next = pe_list;
    pe_list = pe;
#ifdef	DEBUG
    if (psap_log -> ll_events & LLOG_DEBUG) {
	PE     *pep;

	for (pep = &pe_active; p = *pep; pep = &p -> pe_link)
	    if (p == pe) {
		*pep = p -> pe_link;
		break;
	    }
	if (!p)
	    DLOG (psap_log, LLOG_DEBUG,
		  ("WARNING: free'd pe (0x%x) not on active list", pe));
    }
#endif
}
