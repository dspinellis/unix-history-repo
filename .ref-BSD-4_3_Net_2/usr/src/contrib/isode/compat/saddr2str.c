/* saddr2str.c - SSAPaddr to string value */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/compat/RCS/saddr2str.c,v 7.1 91/02/22 09:15:43 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/compat/RCS/saddr2str.c,v 7.1 91/02/22 09:15:43 mrose Interim $
 *
 *
 * $Log:	saddr2str.c,v $
 * Revision 7.1  91/02/22  09:15:43  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  21:23:22  mrose
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
#include "general.h"
#include "manifest.h"
#include "isoaddrs.h"

/*  */

char   *saddr2str (sa)
register struct SSAPaddr *sa;
{
    struct PSAPaddr pas;
    register struct PSAPaddr *pa = &pas;

    bzero ((char *) pa, sizeof *pa);
    pa -> pa_addr = *sa;		/* struct copy */

    return paddr2str (pa, NULLNA);
}
