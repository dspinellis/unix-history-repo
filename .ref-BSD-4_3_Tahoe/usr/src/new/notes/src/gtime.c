#include "parms.h"
#include "structs.h"

#ifdef	RCSIDENT
static char rcsid[] = "$Header: gtime.c,v 1.7 85/01/18 15:12:21 notes Rel $";
#endif	RCSIDENT

/*
 * gettime(whenvec) struct when_f *whenvec; {}
 *
 *    fills in whenvec with the most uptodate time
 *
 *	Rob Kolstad Winter 1980
 *	Modified to split getting and formatting time
 *			Malcolm Slaney	March 1983
 */

/*
 *	4.2 Bsd moved the file!
 */
#ifndef	BSD42
#include 	<time.h>
#else
#include	<sys/time.h>
#endif	!BSD42

gettime (whenvec) struct when_f *whenvec;
{
    long    tvec;

    time (&tvec);					/* get the funky number */
    return (maketime (whenvec, tvec));
}

maketime (whenvec, tvec)
struct when_f  *whenvec;
long    tvec;
{
    struct tm  *ovec;
    struct tm  *localtime ();
    ovec = localtime (&tvec);				/* convert to local time */
    whenvec -> w_mins = ovec -> tm_min;
    whenvec -> w_hours = ovec -> tm_hour;
    whenvec -> w_day = ovec -> tm_mday;
    whenvec -> w_month = ovec -> tm_mon + 1;		/* jan= 0 as supplied, correct it */
    whenvec -> w_year = ovec -> tm_year + 1900;		/* all from CTIME (III) */
    whenvec -> w_gmttime = tvec;
}
