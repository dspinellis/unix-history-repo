/* seq_del.c - delete a member from a sequence */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/psap/RCS/seq_del.c,v 7.1 91/02/22 09:36:53 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/psap/RCS/seq_del.c,v 7.1 91/02/22 09:36:53 mrose Interim $
 *
 *
 * $Log:	seq_del.c,v $
 * Revision 7.1  91/02/22  09:36:53  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  22:13:37  mrose
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

int	seq_del (pe, i)
register PE	pe;
register int	i;
{
    int	    offset;
    register PE	   *p,
		    q;

    for (p = &pe -> pe_cons, offset = 0;
	    q = *p;
	    p = &q -> pe_next, offset = q -> pe_offset)
	if (q -> pe_offset == i) {
	    if (((*p) = q -> pe_next) == NULLPE)
		pe -> pe_cardinal = offset + 1;
	    pe_free (q);
	    return OK;
	}
	else
	    if (q -> pe_offset > i)
		break;
	
    return pe_seterr (pe, PE_ERR_MBER, NOTOK);
}
