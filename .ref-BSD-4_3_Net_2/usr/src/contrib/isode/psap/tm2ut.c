/* tm2ut.c - tm to time string */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/psap/RCS/tm2ut.c,v 7.1 91/02/22 09:37:15 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/psap/RCS/tm2ut.c,v 7.1 91/02/22 09:37:15 mrose Interim $
 *
 *
 * $Log:	tm2ut.c,v $
 * Revision 7.1  91/02/22  09:37:15  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  22:13:54  mrose
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
#ifdef	OSX
#include <sys/time.h>
#endif


#define	YEAR(y)		((y) >= 100 ? (y) : (y) + 1900)

/*  */

void	tm2ut (tm, ut)
register struct tm *tm;
register UTC	ut;
{
    bzero ((char *) ut, sizeof *ut);

    ut -> ut_year = YEAR (tm -> tm_year);
    ut -> ut_mon = tm -> tm_mon + 1;
    ut -> ut_mday = tm -> tm_mday;
    ut -> ut_hour = tm -> tm_hour;
    ut -> ut_min = tm -> tm_min;
    ut -> ut_sec = tm -> tm_sec;
    ut -> ut_zone = 0;
    
    ut -> ut_flags = UT_ZONE | UT_SEC;
}
