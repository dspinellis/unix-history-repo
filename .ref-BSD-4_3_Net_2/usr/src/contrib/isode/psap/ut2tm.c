/* ut2tm.c - time string to tm */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/psap/RCS/ut2tm.c,v 7.1 91/02/22 09:37:17 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/psap/RCS/ut2tm.c,v 7.1 91/02/22 09:37:17 mrose Interim $
 *
 *
 * $Log:	ut2tm.c,v $
 * Revision 7.1  91/02/22  09:37:17  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  22:13:56  mrose
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


#define	UNYEAR(y)	((y) < 1900 || (y) > 1999 ? (y) : (y) - 1900)

extern int dmsize[];

/*  */

struct tm *ut2tm (ut)
register UTC	ut;
{
    static struct tm   tms;
    register struct tm *tm = &tms;

    bzero ((char *) tm, sizeof *tm);

    tm -> tm_sec = ut -> ut_sec;
    tm -> tm_min = ut -> ut_min;
    tm -> tm_hour = ut -> ut_hour;
    tm -> tm_mday = ut -> ut_mday;
    tm -> tm_mon = ut -> ut_mon - 1;
    tm -> tm_year = UNYEAR (ut -> ut_year);
    tm -> tm_wday = makewkday (ut);
    tm -> tm_yday = tm -> tm_isdst = 0;

    tm -> tm_hour -= ut -> ut_zone / 60, tm -> tm_min -= ut -> ut_zone % 60;
    if (tm -> tm_min < 0)
	tm -> tm_hour--, tm -> tm_min += 60;
    else
	if (tm -> tm_min > 59)
	    tm -> tm_hour++, tm -> tm_min -= 60;

/* this ignores odditites in February... */
    if (tm -> tm_hour < 0) {
	tm -> tm_mday++, tm -> tm_hour += 24;
	if (tm -> tm_mday > dmsize[tm -> tm_mon]) {
	    tm -> tm_mon++, tm -> tm_mday = 1;
	    if (tm -> tm_mon > 11)
		tm -> tm_year++, tm -> tm_mon = 0;
	}
	
    }
    else
	if (tm -> tm_hour > 23) {
	    tm -> tm_mday--, tm -> tm_hour -= 24;
	    if (tm -> tm_mday < 1) {
		tm -> tm_mday = dmsize[--tm -> tm_mon];
		if (tm -> tm_mon < 0)
		    tm -> tm_year--, tm -> tm_mon = 11;
	    }
	}

    return tm;
}

/*  */

#define	dysize(y)	\
	(((y) % 4) ? 365 : (((y) % 100) ? 366 : (((y) % 400) ? 365 : 366)))

#define	YEAR(y)		((y) >= 100 ? (y) : (y) + 1900)


static  int makewkday (ut)
UTC     ut;
{
    int     d,
	    mon,
	    year;

    mon = ut -> ut_mon;
    year = YEAR (ut -> ut_year);
    d = 4 + year + (year + 3) / 4;

    if (year > 1800) {
	d -= (year - 1701) / 100;
	d += (year - 1601) / 400;
    }
    if (year > 1752)
	d += 3;
    if (dysize (year) == 366 && mon > 3)
	d++;
    while (--mon >= 0)
	d += dmsize[mon - 1];
    d += ut -> ut_mday - 1;

    return (d % 7);
}
