#include	"parms.h"
#include	"structs.h"

#ifdef	RCSIDENT
static char rcsid[] = "$Header: prtind.c,v 1.7.0.1 85/03/18 20:56:33 notes Rel $";
#endif	RCSIDENT

/*
 *	print the index page for a notefile
 *
 *	Original author: Rob Kolstad	Winter, 1980.
 *	Modifications:	Ray Essick	June 1981.
 *
 */


prntind (io, firstdis, lastdis)
struct io_f *io;
int    *firstdis,
       *lastdis;
{
    struct note_f   note;
    struct when_f   date;
    char    buf[NAMESZ + SYSSZ + 2];			/* hold sys!author for truncation */
    int     atrow;					/* printing row counter */
    int     lyr,
            lday,
            lmon;					/* so we know if need to reprint date */
    int     i;

    erase ();
    at (1, 2);
    printf ("%s", io -> descr.d_title);			/* dump title */
    gettime (&date);
    at (1, 58);
    prdate (&date);

    if (io -> descr.d_stat & ISARCH)
    {
	at (2, 2);
#ifdef	WRITEARCH					/* archive writes? */
	printf ("[ARCHIVE]");
#else	! WRITEARCH
	printf ("[ARCHIVE - NO WRITES]");
#endif	WRITEARCH
    }

    atrow = 5;						/* start printing here */
    lyr = lday = lmon = 0;				/* unknown prev date */
    if (*firstdis > io -> descr.d_nnote - Nindex + 1)
	*firstdis = io -> descr.d_nnote - Nindex + 1;
    if (*firstdis < 1)
	*firstdis = 1;
    *lastdis = *firstdis + Nindex - 1;
    for (i = *firstdis; (i <= *lastdis) & (i <= io -> descr.d_nnote); i++)
    {							/* which does not execute for empty file */
	getnrec (io, i, &note);
	if (note.n_stat & DELETED)
	{
	    if (++(*lastdis) > io -> descr.d_nnote)
		*lastdis = io -> descr.d_nnote;
	    continue;					/* deleted note */
	}
	if (note.n_rcvd.w_year != lyr ||
		note.n_rcvd.w_month != lmon ||
		note.n_rcvd.w_day != lday)		/* need to print date? */
	{
	    at (atrow, 1);
	    printf ("%d/%d", lmon = note.n_rcvd.w_month, lday = note.n_rcvd.w_day);
	    if (note.n_rcvd.w_year != lyr)
		printf ("/%02d", (lyr = note.n_rcvd.w_year) % 100);
	}
	at (atrow, 10);
	printf ("%3d", i);
	if (note.n_stat & DIRMES)
	    printf ("*");
	else
	    printf (" ");
	clearerr (stdout);
	fwrite (note.ntitle, 1, strlen (note.ntitle), stdout);
	if (note.n_nresp != 0)
	{
	    at (atrow, 10 + 4 + TITLEN + 1);
	    printf ("%3d", note.n_nresp);
	}
	at (atrow, 10 + 4 + TITLEN + 1 + 3 + 1);
	if (strcmp (Authsystem, note.n_auth.asystem) != 0 &&
		strcmp ("Anonymous", note.n_auth.aname) != 0)
	{
#ifdef	USERHOST
	    sprintf (buf, "%s@%s", note.n_auth.aname, note.n_auth.asystem);
#else
	    sprintf (buf, "%s!%s", note.n_auth.asystem, note.n_auth.aname);
#endif	USERHOST
	}
	else
	    sprintf (buf, "%s", note.n_auth.aname);

	buf[26] = '\0';					/* don't overflow line */
	printf ("%s", buf);
	if (isinput ())
	    return;					/* he typed */
	atrow++;
	if (intflag)					/* did he want out? */
	{
	    *lastdis = i;				/* show last displayed */
	    intflag = 0;				/* don't do same one twice */
	}
    }

    if (*lastdis >= io -> descr.d_nnote)
    {
	at (++atrow, 14);
	printf ("**** End of Notes ****");
    }

    at (atrow + 2, 25);
    printf ("- - - - - - - - - - - - - - -");

}
