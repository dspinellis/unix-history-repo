#include "parms.h"
#include "structs.h"

#ifdef	RCSIDENT
static char rcsid[] = "$Header: limindx.c,v 1.7.0.2 85/09/09 18:32:46 notes Rel $";
#endif	RCSIDENT

/*
 *	limindx(io) struct io_f *io;
 *	runs a limited form of the index and prntind functions.
 *	its purpose is to come up with a note number for forwarding
 *	notes as responses.
 *
 *	Returns the note number picked. 
 *		0 means no note.
 *
 *	Original coding:	Ray Essick	January 1982
 *
 */

limindx (io)
struct io_f *io;
{
    int     firstdis,
            lastdis;
    char    c;
    int     i,
            znote,
            zresp;
    struct note_f   note;
    char    buf[NAMESZ + SYSSZ + 2];			/* for display */

    firstdis = io -> descr.d_nnote - Nindex + 1;	/* start at the end of the file */
    while (1)
    {
	prntind (io, &firstdis, &lastdis);		/* show the page */
	at (2, 28);
	printf ("---- Limited Index ----");
	at (0, 1);
	c = gchar ();
	printf ("\b \b");
	switch (c)
	{
	    case '?': 
	    case 'h': 
		help (LIMHLP);
		break;

	    case '\r': 
	    case '\n': 
	    case ' ': 
	    case '+': 					/* advance 1 page */
		firstdis = lastdis;
		break;

	    case '-': 					/* backwards 1 page */
	    case '\b': 
		firstdis -= Nindex;
		break;

	    case '=': 					/* back to the start */
		firstdis = 1;
		break;

	    case '*': 					/* last page of index */
		firstdis = io -> descr.d_nnote - Nindex + 1;
		break;

	    case '!': 					/* give him a shell */
		gshell ();
		break;

	    case 'q': 					/* leave, giving up */
	    case 'Q': 					/* and others .. */
#ifdef	K_KEY
	    case 'k': 
	    case 'K': 
#endif	K_KEY
		return 0;

	    case 'x': 					/* search for title */
	    case 'X': 
		i = tsearch (io, lastdis, c == 'x');	/* assume lies before here */
		if (i > 0)
		    firstdis = i;			/* set him on that page */
		continue;				/* otherwise get another key */

	    case 'a': 
	    case 'A': 					/* author search from current spot */
		znote = lastdis;
		zresp = 0;				/* start at the correct place */
		do
		{
		    i = asearch (io, &znote, &zresp, (c == 'a'));
							/* look */
		    if (i > 0)
		    {
			if (zresp != 0)
			    continue;			/* not a 'note' */
			firstdis = znote;		/* go there */
			break;				/* out of this loop */
		    }
		}
		while (i > 0);				/* until not found */
		continue;				/* get another command */

	    case '0': 					/* pick a note */
	    case '1': 
	    case '2': 
	    case '3': 
	    case '4': 
	    case '5': 
	    case '6': 
	    case '7': 
	    case '8': 
	    case '9': 
		printf ("  Note number > ");
		znote = getnum (c);
		if (znote <= io -> descr.d_nnote)	/* if range ok */
		{
		    /* 
		     * print the index line for this note...
		     */
		    getnrec (io, znote, &note);		/* get descriptor */
#define	INDEXROW	-2
		    at (INDEXROW, 1);
		    printf ("%d/%d/%02d", note.n_rcvd.w_month,
			    note.n_rcvd.w_day, note.n_rcvd.w_year % 100);
		    at (INDEXROW, 10);
		    printf ("%3d", znote);
		    if (note.n_stat & DIRMES)
			printf ("*");
		    else
			printf (" ");
		    clearerr (stdout);
		    fwrite (note.ntitle, 1, strlen (note.ntitle), stdout);
		    if (note.n_nresp != 0)
		    {
			at (INDEXROW, 10 + 4 + TITLEN + 1);
			printf ("%3d", note.n_nresp);
		    }
		    at (INDEXROW, 10 + 4 + TITLEN + 1 + 3 + 1);
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

		    buf[26] = '\0';			/* don't overflow line */
		    printf ("%s", buf);
		    at (-1, 1);				/* now ask him */
		    printf ("Do you really want note %d? ", znote);
		    if (askyn (" (y/n) ") == 'y')	/* he wants it */
			return znote;
		}
		else
		{
		    at (0, PROMPTMSGX);
		    printf ("Note %d doesn't exist");
		    continue;
		}
	}
    }
}
