/* oid_cmp.c - compare two object identifiers */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/psap/RCS/oid_cmp.c,v 7.1 91/02/22 09:35:56 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/psap/RCS/oid_cmp.c,v 7.1 91/02/22 09:35:56 mrose Interim $
 *
 *
 * $Log:	oid_cmp.c,v $
 * Revision 7.1  91/02/22  09:35:56  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  22:12:50  mrose
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

int	oid_cmp (p, q)
register OID	p,
		q;
{
    if (p == NULLOID)
	return (q ? -1 : 0);

    return elem_cmp (p -> oid_elements, p -> oid_nelem,
		     q -> oid_elements, q -> oid_nelem);
}

/*  */

int	elem_cmp (ip, i, jp, j)
register int   i,
	       j;
register unsigned int *ip,
		      *jp;
{
    while (i > 0) {
	if (j == 0)
	    return 1;
	if (*ip > *jp)
	    return 1;
	else
	    if (*ip < *jp)
		return (-1);

	ip++, i--;
	jp++, j--;
    }
    return (j == 0 ? 0 : -1);
}
