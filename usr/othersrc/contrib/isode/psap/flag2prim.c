/* flag2prim.c - boolean to presentation element */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/psap/RCS/flag2prim.c,v 7.1 91/02/22 09:35:38 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/psap/RCS/flag2prim.c,v 7.1 91/02/22 09:35:38 mrose Interim $
 *
 *
 * $Log:	flag2prim.c,v $
 * Revision 7.1  91/02/22  09:35:38  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  22:12:36  mrose
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

PE	flag2prim (b, class, id)
register int b;
PElementClass	class;
PElementID	id;
{
    register PE	    pe;

    if ((pe = pe_alloc (class, PE_FORM_PRIM, id)) == NULLPE)
	return NULLPE;

    if ((pe -> pe_prim = PEDalloc (pe -> pe_len = 1)) == NULLPED) {
	pe_free (pe);
	return NULLPE;
    }

    *pe -> pe_prim = b ? 0xff : 0x00;

    return pe;
}
