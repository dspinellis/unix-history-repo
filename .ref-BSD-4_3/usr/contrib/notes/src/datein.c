#include "parms.h"
#include "structs.h"

#ifdef	RCSIDENT
static char rcsid[] = "$Header: datein.c,v 1.7.0.1 85/07/31 14:12:38 notes Rel $";
#endif	RCSIDENT

/*
 *	getdate - parses the date handed as a string. Format is assumed
 *	to be that of CTIME(III).
 *		Ray Essick		Feb 1982
 *
 *	Augmented to understand about lots of different formats -
 *	and make use of the getdate(III) routine to parse an
 *	almost arbitrary date.
 *
 *	If the routine is unable to parse a time, the current time
 *	is returned.  If the parsed time is in the future, the
 *	current time is returned.
 *
 *	Returns:		0	succesfully parsed past date
 *				-1	unsucessful parse
 *					returned current time
 *				-2	unsuccessful parse
 *					returned a future time
 *
 */

extern char *mnames[];

parsetime (line, date)
char   *line;
struct when_f  *date;
{
    char    month[100];
    char    weekday[100];				/* day of week */
    char    zonename[100];				/* time zone */
    int     day,
            hour,
            min,
            sec,
            year;
    int     count;
    long    timenow;					/* current time */
    long    xtime;					/* parsed time */
    struct when_f   now;				/* in structure */


    gettime (date);					/* load current */
    gettime (&now);					/* load current */
    time (&timenow);					/* seconds GMT */

#ifdef	notdef
    /* 
     * apparently there are other formats close enough to ctime
     * to fool the sscanf() call into thinking it has succeeded
     * when in fact it hasn't.  So we just fall directly
     * into the getdate() call.
     */

    /* 
     *	First, let's see if the string is CTIME(III) format.
     */
    if (sscanf (line, "%s %s %hd %hd:%hd:%hd %hd", weekday, month,
		&day, &hour, &min, &sec, &year) == 7)
    {							/* yay! */

	for (count = 12; count > 0; count--)
	    if (strcmp (mnames[count], month) == 0)
		break;
	date -> w_month = count;			/* place what we got */
	date -> w_day = day;
	date -> w_hours = hour;
	date -> w_mins = min;
	date -> w_year = year;
	date -> w_gmttime = 0;
	/* 
	 *	We should check that it isn't in the future here
	 */
	return 0;
    }

#endif

    /* 
     *	Ok, now it's time to call in the big artillery -- GETDATE
     */

    xtime = getdate (line, (struct timeb *) NULL);	/* parse */
    if (xtime < 0)					/* success? */
	return (-1);					/* failure */
    maketime (date, (long) xtime);			/* encode */
    if (xtime > timenow)				/* future shock */
	return (-2);
    return (0);

}
