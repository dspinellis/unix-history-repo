#include	"parms.h"
#include	"structs.h"

#ifdef	RCSIDENT
static char rcsid[] = "$Header: gdate.c,v 1.7.0.1 85/09/09 18:32:11 notes Rel $";
#endif	RCSIDENT


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
 *	Rewritten to use the parsetime() routine which understands
 *	about converting ASCII time specifications into a UNIX
 *	Internal time.  
 *		-- Ray Essick, March 1984
 *
 */

#define	TIMELEN	40					/* longest time spec */

gdate (date) struct when_f *date;
{
    char    datin[TIMELEN + 1],
            fmt[DATELEN];
    register int    i,
                    pass;
    struct when_f   tmpdate;				/* hold it */

    pass = 0;
    while (1)
    {
	sprdate (date, fmt);				/* current setting */
	at (0, PROMPTMSGX);
	printf ("Set to read notes since: %-*s", DATELEN, fmt);
	at (-1, 10);
	printf ("New Date > %*s", TIMELEN, " ");
	at (-1, 21);
	for (i = 0; i < TIMELEN; i++)
	    datin[i] = ' ';
	if (gline (datin, TIMELEN) == 1)
	{						/* empty line */
	    at (-1, 10);
	    printf ("%*s", 23 + TIMELEN, " ");		/* space padding */
	    if (pass)
		return 0;				/* ok */
	    else
		return (-1);				/* 1st pass return */
	}
	switch (parsetime (datin, &tmpdate))		/* parsed? */
	{
	    case 0: 					/* worked */
		*date = tmpdate;			/* transfer it */
		pass++;					/* count passes */
		break;

	    default: 					/* failures */
		putc ('\07', stdout);			/* bark at him */
		break;
	}

    }
/*	yes, we know about the fact that this statement is unreachable! */
    return 0;						/* never executed */
}
