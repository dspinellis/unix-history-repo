/* ps_flush.c - flush a presentation stream */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/psap/RCS/ps_flush.c,v 7.1 91/02/22 09:36:33 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/psap/RCS/ps_flush.c,v 7.1 91/02/22 09:36:33 mrose Interim $
 *
 *
 * $Log:	ps_flush.c,v $
 * Revision 7.1  91/02/22  09:36:33  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  22:13:21  mrose
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

int	ps_flush (ps)
register PS	ps;
{
    if (ps -> ps_flushP)
	return (*ps -> ps_flushP) (ps);

    return OK;
}
