/* seq_find.c - find an element in a sequence */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/psap/RCS/seq_find.c,v 7.1 91/02/22 09:36:54 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/psap/RCS/seq_find.c,v 7.1 91/02/22 09:36:54 mrose Interim $
 *
 *
 * $Log:	seq_find.c,v $
 * Revision 7.1  91/02/22  09:36:54  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  22:13:38  mrose
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

PE	seq_find (pe, i)
register PE	pe;
register int	i;
{
    register PE	    p;

    if (i >= pe -> pe_cardinal)
	return pe_seterr (pe, PE_ERR_MBER, NULLPE);

    for (p = pe -> pe_cons; p; p = p -> pe_next)
	if (p -> pe_offset == i)
	    break;

    return p;
}
