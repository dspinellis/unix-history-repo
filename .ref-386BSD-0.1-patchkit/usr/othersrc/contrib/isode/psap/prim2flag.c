/* prim2flag.c - presentation element to boolean */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/psap/RCS/prim2flag.c,v 7.1 91/02/22 09:36:21 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/psap/RCS/prim2flag.c,v 7.1 91/02/22 09:36:21 mrose Interim $
 *
 *
 * $Log:	prim2flag.c,v $
 * Revision 7.1  91/02/22  09:36:21  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  22:13:10  mrose
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

int	prim2flag (pe)
register PE	pe;
{
    if (pe -> pe_form != PE_FORM_PRIM
	    || pe -> pe_prim == NULLPED
	    || pe -> pe_len == 0)
	return pe_seterr (pe, PE_ERR_PRIM, NOTOK);

    return (*pe -> pe_prim != 0x00);
}
