/* pe_expunge.c - expunge a PE */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/psap/RCS/pe_expunge.c,v 7.2 91/02/22 09:36:13 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/psap/RCS/pe_expunge.c,v 7.2 91/02/22 09:36:13 mrose Interim $
 *
 *
 * $Log:	pe_expunge.c,v $
 * Revision 7.2  91/02/22  09:36:13  mrose
 * Interim 6.8
 * 
 * Revision 7.1  91/02/12  18:32:46  mrose
 * upate
 * 
 * Revision 7.0  89/11/23  22:13:03  mrose
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

PE	pe_expunge (pe, r)
PE	pe,
	r;
{
    if (r) {
	if (pe == r)
	    return r;

	if (pe_extract (pe, r))
	    if (pe -> pe_realbase && !r -> pe_realbase) {
		r -> pe_realbase = pe -> pe_realbase;
		pe -> pe_realbase = NULL;
	    }

	r -> pe_refcnt++;
    }

    pe_free (pe);

    return r;
}
