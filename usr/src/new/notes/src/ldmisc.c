#include "parms.h"
#include "structs.h"

#ifdef	RCSIDENT
static char rcsid[] = "$Header: ldmisc.c,v 1.7 85/01/18 15:14:52 notes Rel $";
#endif	RCSIDENT

/*	misc routines used in loading a generic form back into the notefile
 *	includes routines to suck in a line, parse a unique identifier and
 *	all sorts of other fun stuff
 *
 *	Ray Essick	december 1981
 */

/*
 *	timein(line, atime) - grab a time from a coded line.
 *	makes sure that all the pieces of a time come in.
 */

timein (line, atime)
char   *line;
struct when_f  *atime;
{
    int     count;

    count = sscanf (line, "%hd:%hd:%hd:%hd:%hd:%ld:", &atime -> w_year, &atime -> w_month,
	    &atime -> w_day, &atime -> w_hours, &atime -> w_mins,
	    &atime -> w_gmttime);
    if (count < 5)
    {
	printf ("timein: bad date read, set to now\n");
	gettime (atime);
    }
    else
	if (count < 6)					/* no gmttime */
	    atime -> w_gmttime = 0;			/* empty */
}

/*
 *	initnote(&note_f)
 *
 *	zero most of the fields of a note_f
 *
 */

initnote (note)
struct note_f  *note;
{
    register int    i;
    strcpy (note -> n_id.sys, "");			/* unique id */
    note -> n_id.uniqid = 0;
    note -> n_nresp = 0;				/* no responses */
    note -> ntitle[0] = '\0';				/* empty title */
    strcpy (note -> n_auth.aname, "Unknown");		/* author */
    note -> n_auth.aid = Anonuid;
    gettime (&note -> n_date);				/* date written */
    gettime (&note -> n_rcvd);
    gettime (&note -> n_lmod);
    strcpy (note -> n_from, "");			/* from where */
    note -> n_rindx = 0;				/* no responses */
    note -> n_stat = 0;					/* no status bits */
}
