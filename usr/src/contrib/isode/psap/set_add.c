/* set_add.c - add member to set */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/psap/RCS/set_add.c,v 7.1 91/02/22 09:36:56 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/psap/RCS/set_add.c,v 7.1 91/02/22 09:36:56 mrose Interim $
 *
 *
 * $Log:	set_add.c,v $
 * Revision 7.1  91/02/22  09:36:56  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  22:13:39  mrose
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

int	set_add (pe, r)
register PE	pe,
		r;
{
    register int     pe_id;
    register PE	    *p,
		    q;

    if (r == NULLPE)
	return pe_seterr (pe, PE_ERR_NMEM, NOTOK);

    pe_id = PE_ID (r -> pe_class, r -> pe_id);
    for (p = &pe -> pe_cons; q = *p; p = &q -> pe_next)
	if (PE_ID (q -> pe_class, q -> pe_id) == pe_id) {
	    r -> pe_next = q -> pe_next;
	    pe_free (q);
	    break;
	}

    *p = r;
    return OK;
}
