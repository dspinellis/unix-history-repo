static char *sccsid = "@(#)datein.c	1.1\t1/23/83";

#include "parms.h"
#include "structs.h"
/*
 *	getdate - parses the date handed as a string. Format is assumed
 *	to be that of CTIME(III).
 *		Ray Essick		Feb 1982
 */

extern char *mnames[];

getdate (line, date)
char   *line;
struct when_f  *date;
{
    char    month[100];			/* changed from 10 to 100 by RLS */
    short day, hour, min, sec, year;
    int     count;

    while(*line++ != ' ');			/* skip day of week */

    gettime(date);				/* if bad will use now */
    if (sscanf(line, "%[^ ]%hd %hd:%hd:%hd %hd", month, &day, &hour, &min, &sec, &year) != 6) {
	return(-1);				/* bad date parsed */
    }

    for (count = 12; count > 0; count--) {
	if (strcmp(mnames[count], month) == 0) {
	    break;
	}
    }
    date->w_month = count;				/* place what we got */
    date->w_day = day;
    date->w_hours = hour;
    date->w_mins = min;
    date->w_year = year;

    return(0);			/* must return a value to match above */
}
