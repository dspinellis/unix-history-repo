static char *sccsid = "@(#)prtind.c	1.1\t1/23/83";

#include "parms.h"
#include "structs.h"

/*
 *	print the index page for a notefile
 *
 *	Original author: Rob Kolstad	Winter, 1980.
 *	Modifications:	Ray Essick	June 1981.
 */
prntind (io, firstdis, lastdis)
struct io_f *io;
int    *firstdis,
       *lastdis;
{
    struct note_f   note;
    struct when_f   date;
    char    buf[NAMESZ + SYSSZ + 2];	/* hold sys!author for truncation */
    int
            atrow;			/* printing row counter */
    int     lyr,
            lday,
            lmon;		/* so we know if need to reprint date */
    int     i;
    long    retval;

    erase ();
    at (1, 2);
    fwrite (io->descr.d_title, 1, len (io->descr.d_title, NNLEN), stdout);
    gettime (&date);				/* screen will look nice */
    at (1, 58);
    prdate (&date);
    atrow = 5;					/* start printing here */
    lyr = lday = lmon = 0;			/* unknown prev date */
    if (*firstdis > io->descr.d_nnote - nindex + 1) {
	*firstdis = io->descr.d_nnote - nindex + 1;
    }
    if (*firstdis < 1) {
	*firstdis = 1;
    }
    *lastdis = *firstdis + nindex - 1;

    for (i = *firstdis; (i <= *lastdis) & (i <= io->descr.d_nnote); i++) {
    			/* which does not execute for empty file */
	getnrec (io, i, &note);
	if (note.n_stat & DELETED) {
	    if (++(*lastdis) > io->descr.d_nnote) {
		*lastdis = io->descr.d_nnote;
	    }
	    continue;					/* deleted note */
	}
	if (note.n_rcvd.w_year != lyr ||
		note.n_rcvd.w_month != lmon ||
		note.n_rcvd.w_day != lday)	/* need to print date? */
	{
	    at (atrow, 1);
	    printf ("%d/%d", lmon = note.n_rcvd.w_month, lday = note.n_rcvd.w_day);
	    if (note.n_rcvd.w_year != lyr) {
		printf ("/%02d", (lyr = note.n_rcvd.w_year) % 100);
	    }
	}

	at (atrow, 10);
	printf ("%3d", i);
	if (note.n_stat & DIRMES) {
	    printf ("*");
	} else {
	    printf (" ");
	}
	fwrite (note.ntitle, 1, len (note.ntitle, TITLEN), stdout);
	if (note.n_nresp != 0) {
	    at (atrow, 10 + 4 + TITLEN + 1);
	    printf ("%3d", note.n_nresp);
	}
	at (atrow, 10 + 4 + TITLEN + 1 + 3 + 1);
	if (strcmp (SYSTEM, note.n_id.sys) != 0 && strcmp ("Anonymous", note.n_auth.aname) != 0)
	    sprintf (buf, "%s!%s", note.n_id.sys, note.n_auth.aname);
	else
	    sprintf (buf, "%s", note.n_auth.aname);

	/* Fix? */
	if (isinput())
		return;

	buf[26] = '\0';			/* don't overflow line */
	printf ("%s", buf);
	atrow++;
    }

    if (*lastdis >= io->descr.d_nnote) {
	at (++atrow, 14);
	printf ("**** End of Notes ****");
    }

    at (atrow + 3, 25);
    printf ("- - - - - - - - - - - - - - -");
}
