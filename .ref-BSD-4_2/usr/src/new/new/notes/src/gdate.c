static char *sccsid = "%W%";

#include "parms.h"
#include "structs.h"

/*
 * this gdate routine reads a "search date" from the keyboard.  This date
 * must be of the form mm/dd or mm/dd/yy (number of digits in each is
 * irrelevant).  it sets the date parameter correctly to the input or
 * returns -1 to indicate failure to complete a date (i.e., a return alone
 * was entered on a line).  If the date is invalid, it must be retyped.
 * I know it is unfortunate that we can't have a plato arrow type judging
 * system here.  I just don't want to go to all the hassle of making one now.
 * perhaps this is an area of improvement for this program.  RK  11/10/80.
 *
 */

#define	TIMELEN	20				/* length of time entry */

gdate (date) struct when_f *date;
{
    char    datin[TIMELEN + 1],
            fmt[DATELEN];
    char buf[BUFSIZ];
    register int    i,
                    pass;
    struct when_f   today;
    int     ih,
            imin,
            im,
            iy,
            id;					/* temporary month/day/year */
    gettime (&today);
    pass = 0;
    while (1) {
	sprdate(date, fmt);			/* show him current setting */
	sprintf(buf, "Set to read notes since: %-*s", DATELEN, fmt);
	at(-1, 1);
	putstr(buf);
	for (i = 0; i < TIMELEN; i++) {
	    datin[i] = ' ';
	}
	prompt("New Date (m/d/y h:m) > ");
	i = gline(datin, TIMELEN);
	at(-1, 1);
	clear_eol();
	if (i == 1) {
	    if (pass) {
		return(0);
	    } else {
		return(-1);				/* return on 1st pass */
	    }
	}
	i = sscanf (datin, "%d/%d/%d %d:%d", &im, &id, &iy, &ih, &imin);
	switch (i) {
	    case 5:				/* specified the whole shot */
		if (imin > 59 || imin < 0) {
		    goto err;				/* bad news */
		}
		date->w_mins = imin;

	    case 4: 			/* only specified through hours */
		if (ih > 23 || ih < 0) {
		    goto err;
		}
		date->w_hours = ih;
	    case 3: 				/* specified through year */
		iy += 1900;			/* adjust the date */
		if (iy < 1970 || iy > 1999) {
		    goto err;
		}
		date->w_year = iy;
	    case 2: 				/* specified the day */
		if (id < 1 || id > 31) {
		    goto err;
		}
		date->w_day = id;
	    case 1:				/* just specifed month */
		if (im < 1 || im > 12) {
		    goto err;
		}
		date->w_month = im;
		break;

	    default: 
		i = 0;					/* bad news */
		break;
	}

	if (i < 3) {		/* just did date, default hours/mins/year */
	    date->w_year = today.w_year;
	    date->w_hours = date->w_mins = 0;
	}
	pass = 1;			/* have parsed at least once */
	continue;

err: 							/* bad entry */
	printf("\07");
    }
}
