/* ps_prime.c - prime a presentation stream */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/psap/RCS/ps_prime.c,v 7.2 91/02/22 09:36:39 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/psap/RCS/ps_prime.c,v 7.2 91/02/22 09:36:39 mrose Interim $
 *
 *
 * $Log:	ps_prime.c,v $
 * Revision 7.2  91/02/22  09:36:39  mrose
 * Interim 6.8
 * 
 * Revision 7.1  91/01/07  12:40:37  mrose
 * update
 * 
 * Revision 7.0  89/11/23  22:13:25  mrose
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

int	ps_prime (ps, waiting)
register PS	ps;
int	waiting;
{
    if (ps -> ps_primeP)
	return (*ps -> ps_primeP) (ps, waiting);

    return OK;
}
