static char *sccsid = "@(#)ldmisc.c	1.1\t1/23/83";

#include "parms.h"
#include "structs.h"
/*	misc routines used in loading a generic form back into the notefile
 *	includes routines to suck in a line, parse a unique identifier and
 *	all sorts of other fun stuff
 *
 *	Ray Essick	december 1981
 */

/*
 *	numin(line) - suck in a number, return it
 */

long numin (line)
char   *line;
{
    register int    sign;
    long count;
    register char   c;

    count = 0;
    sign = 1;
    if (*line == '-') {
	sign = -1;
	line++;
    }
    while ((c = (*line++)) >= '0' && c <= '9') {
	count = count * 10 + c - '0';
    }

    return((long) (sign * count));
}

sukline (zfile, line)
FILE * zfile;
char   *line;
{
    int     c;
    while ((c = getc (zfile)) != '\n' && c != -1) {
	*line++ = c;
    }
    *line++ = '\0';			/* add a null to the end */
    return(c);				/* return the last character read */
}

/*
 *	parseid(line, id) - grab a unique id from the line and
 *	return it to the specified pointer 
 *
 *	WARNING:	this routine assumes that everyone in the world
 *			has the same max for name lengths. If a name
 *			comes through that is longer than the max,
 *			this routine will croak..(probably taking others
 *			with it).
 */

parseid (line, id)
char   *line;
struct id_f *id;
{
    char   *p,
           *q;

    q = id->sys;
    p = line;
    while ((*q++ = *p++) != ':');	/* move the system name */
    *--q = '\0';			/* overwrite colon and null terminate */

    id->uniqid = numin (p);			/* set the user id */
}

/*
 *	timein(line, atime) - grab a time from a coded line.
 *	makes sure that all the pieces of a time come in.
 */

timein (line, atime)
char   *line;
struct when_f  *atime;
{
    int     count;

    count = sscanf (line, "%hd:%hd:%hd:%hd:%hd", &atime->w_year, 
	    &atime->w_month, &atime->w_day, &atime->w_hours, &atime->w_mins);
    if (count != 5) {
	printf ("timein: bad date read, set to now\n");
	gettime (atime);
    }
}
