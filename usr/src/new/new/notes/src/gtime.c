static char *sccsid = "@(#)gtime.c	1.1\t1/23/83";

#include "parms.h"
#include "structs.h"
#include <sys/time.h>

/*
 * gettime(whenvec) struct when_f *whenvec; {}
 *
 *    fills in whenvec with the most uptodate time
 *
 *	Rob Kolstad Winter 1980
 */

gettime (whenvec) struct when_f *whenvec;
{
    long tvec;
    struct tm  *ovec;
    struct tm  *localtime ();

    time (&tvec);				/* get the funky number */
    ovec = localtime (&tvec);			/* convert to local time */
    whenvec->w_mins = ovec->tm_min;
    whenvec->w_hours = ovec->tm_hour;
    whenvec->w_day = ovec->tm_mday;
    whenvec->w_month = ovec->tm_mon + 1;    /* jan= 0 as supplied, correct it */
    whenvec->w_year = ovec->tm_year + 1900;	/* all from CTIME (III) */
}
