/* str2prim.c - octet string to presentation element */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/psap/RCS/str2prim.c,v 7.1 91/02/22 09:37:06 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/psap/RCS/str2prim.c,v 7.1 91/02/22 09:37:06 mrose Interim $
 *
 *
 * $Log:	str2prim.c,v $
 * Revision 7.1  91/02/22  09:37:06  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  22:13:48  mrose
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

PE	str2prim (s, len, class, id)
register char *s;
register int len;
PElementClass	class;
PElementID	id;
{
    register PE	    pe;

    if ((pe = pe_alloc (class, PE_FORM_PRIM, id)) == NULLPE)
	return NULLPE;

    if (len && (pe -> pe_prim = PEDalloc (pe -> pe_len = len)) == NULLPED) {
	pe_free (pe);
	return NULLPE;
    }

    if (s)
	PEDcpy (s, pe -> pe_prim, len);

    return pe;
}
