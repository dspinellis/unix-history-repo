/* prim2flag.c - presentation element to set */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/psap/RCS/prim2set.c,v 7.1 91/02/22 09:36:26 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/psap/RCS/prim2set.c,v 7.1 91/02/22 09:36:26 mrose Interim $
 *
 *
 * $Log:	prim2set.c,v $
 * Revision 7.1  91/02/22  09:36:26  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  22:13:15  mrose
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

PE	prim2set (pe)
register PE	pe;
{
    register int    i;
    register PE	    p;

    if (pe -> pe_form != PE_FORM_CONS)
	return pe_seterr (pe, PE_ERR_CONS, NULLPE);

    for (i = 0, p = pe -> pe_cons; p; p = p -> pe_next)
	p -> pe_offset = i++;

    pe -> pe_cardinal = i;

    return pe;
}
