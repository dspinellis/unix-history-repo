#include	"parms.h"
#include	"structs.h"

#ifdef	RCSIDENT
static char rcsid[] = "$Header: dsply.c,v 1.7.0.2 85/03/18 20:54:06 notes Rel $";
#endif	RCSIDENT


/*
 * dspnote(io,note) struct io_f *io; struct note_f *note;
 *
 *    displays a completely formatted note on the screen.  It calls
 *    gettrec to get the text of the note.
 *
 * dspresp(io,note,resp,phys,logical) is a little trickier.
 *
 *   it prints out the response to <note> from response form <resp>
 *   (at index of <resp> of <phys>).  <logical> is the logical number
 *   of the note and is displayed.
 *
 *	Original coding:	Rob Kolstad	Winter 1980
 *	Modified:		Ray Essick	December 1981
 *		- reordered display so went top to bottom, left to right
 *		- to facilitate use on dumb terminals (or tty's)
 */

static char *Anon = "Anonymous";
static char *Fromnews = "(From News system)";
static char *Continued = "-- more  %d%% --";


dspnote (io, note, notenum)
struct io_f *io;
struct note_f  *note;
{
    int     c,						/* character temp */
            Rotate;					/* whether rotated */
    long    where;					/* for seeking */
    struct dsply_f  dispbuf;
    long    pagecnt[PAGESAV + 1];
    int     pagesout;


    Rotate = 0;						/* default non-rotated */
    io -> nnotread++;					/* bump count of notes read */
    pagesout = 0;
    pagecnt[pagesout] = 0;

    if (note -> n_addr.addr == 0)
	dispbuf.d_head.textlen = 0;			/* empty text */
    else
    {
	x (lseek (io -> fidtxt, note -> n_addr.addr, 0) != note -> n_addr.addr,
		"dsply: bad note seek");
	dispbuf.d_head = note -> n_addr;		/* get header info */
    }

    dispbuf.outcount = dispbuf.optr = dispbuf.olim = 0;	/* fix buffer */
    do							/* always show the first page */
							/* display loop */
    {
	erase ();					/* write header crap */
	at (1, 1);
	if (notenum != 0)
	{
	    if (dispbuf.outcount)			/* nth page */
		printf ("[Continued] ");
	    printf ("Note %d", notenum);
	}
	center (io -> descr.d_title, NNLEN, 1, 40 - NNLEN / 2);
	if (note -> n_nresp > 0)
	{
	    at (1, 67);
	    printf ("%d response", note -> n_nresp);
	    if (note -> n_nresp > 1)
		printf ("s");
	}
	at (2, 1);
	printf ("%s", note -> n_auth.aname);
	center (note -> ntitle, TITLEN, 2, 40 - TITLEN / 2);
	at (2, 59);
	prdate (&note -> n_date);
	if (strcmp (Authsystem, note -> n_auth.asystem) && strcmp (Anon, note -> n_auth.aname))
	{
	    at (3, 1);
	    printf ("(at %s)", note -> n_auth.asystem);	/* print system name if not local */
	}
	if (note -> n_stat & DIRMES)
	    center (io -> descr.d_drmes, DMLEN, 3, 40 - DMLEN / 2);
	else
	    if (note -> n_stat & ORPHND)
	    {
		at (3, 29);
		printf ("-- (Foster Parent) --");
	    }
	    else
		if (note -> n_stat & WRITONLY)
		{
		    at (3, 31);
		    printf ("-- (Write Only) --");
		}
	if (note -> n_stat & FRMNEWS)
	{
	    at (3, 59);
	    printf ("%s", Fromnews);
	}
	if (dispbuf.d_head.textlen == 0)
	{
	    c = (-1);
	    break;					/* header is it for empties */
	}
	at (4, 1);
	putc ('\n', stdout);				/* make sure soft-tabs work */
	c = showit (io, &dispbuf, Rotate);
	switch (c)
	{
	    case ' ': 					/* wants the next page */
		if (pagesout < PAGESAV)
		    pagecnt[++pagesout] = dispbuf.outcount;
							/* save new start */
		break;

	    case '-': 					/* go back to previous */
	    case '\b': 					/* backspace has same function */
		if (pagesout-- == 0)
		    return c;				/* pass the buck */
		goto replot;				/* else fall into replot */

#ifdef	ROTATE
	    case 'R': 					/* rot-13 the text */
		Rotate = !Rotate;			/* toggle */
							/* and fall into re-plot */
#endif	ROTATE
	replot: 					/* nasty goto label */
	    case 'r': 
	    case '\014': 				/* control-L too */
		dispbuf.optr = dispbuf.olim = 0;
		dispbuf.outcount = pagecnt[pagesout];
		where = dispbuf.d_head.addr + dispbuf.outcount;
		x (lseek (io -> fidtxt, where, 0) < 0, "dspnote: bad seek");
		break;					/* are all set now */


	    default: 					/* pass the buck */
		return c;
	}
    } while (dispbuf.outcount < dispbuf.d_head.textlen);

    return c;						/* didn't field a command */
}

/*
 *	Code to print the header and manage the paging of a response
 *	It calls the "showit" routine to print pages of text
 */

dspresp (io, note, resp, phys, logical, notenum)
struct io_f *io;
struct note_f  *note;
struct resp_f  *resp;
{
    int     c;
    int     Rotate;					/* whether rotated */
    struct dsply_f  dispbuf;

    long    where,
            pagecnt[PAGESAV + 1];			/* stack for backing up */
    int     pagesout;

    Rotate = 0;						/* default non-rotated */
    io -> nrspread++;					/* bump count or responses read */
    pagesout = 0;
    pagecnt[pagesout] = 0;


    if (resp -> r_addr[phys].addr == 0)
	dispbuf.d_head.textlen = 0;			/* so can get a header */
    else
    {
	x (lseek (io -> fidtxt, resp -> r_addr[phys].addr, 0) != resp -> r_addr[phys].addr,
		"dspresp: bad resp seek");
	dispbuf.d_head = resp -> r_addr[phys];		/* get header info */
    }

    dispbuf.outcount = dispbuf.optr = dispbuf.olim = 0;	/* fix buffer */
    do							/* always make 1 pass */
    {
	erase ();					/* paint the header */
	at (1, 1);
	if (dispbuf.outcount)				/* nth page */
	    printf ("[Continued] ");
	printf ("Note %d", notenum);
	center (io -> descr.d_title, NNLEN, 1, 40 - NNLEN / 2);
	at (2, 1);
	printf ("%s", resp -> r_auth[phys].aname);
	at (2, 31);
	printf ("Response %2d of %2d", logical, note -> n_nresp);
	at (2, 59);
	prdate (&resp -> r_when[phys]);
	if (strcmp (Authsystem, resp -> r_auth[phys].asystem) != 0 && strcmp (Anon, resp -> r_auth[phys].aname) != 0)
	{
	    at (3, 1);
	    printf ("(at %s)", resp -> r_auth[phys].asystem);/* print sys name if not here */
	}
	if (resp -> r_stat[phys] & DIRMES)
	    center (io -> descr.d_drmes, DMLEN, 3, 40 - DMLEN / 2);
	if (resp -> r_stat[phys] & FRMNEWS)
	{
	    at (3, 59);
	    printf ("%s", Fromnews);
	}
	if (dispbuf.d_head.textlen == 0)
	{
	    c = (-1);
	    break;					/* header is all for empties */
	}
	at (4, 1);
	putc ('\n', stdout);				/* make sure soft-tabs work */
	c = showit (io, &dispbuf, Rotate);
	switch (c)
	{
	    case ' ': 					/* wants the next page */
		if (pagesout < PAGESAV)
		    pagecnt[++pagesout] = dispbuf.outcount;
							/* save new start */
		break;

	    case '-': 					/* go back to previous */
	    case '\b': 					/* backspace does the same thing */
		if (pagesout-- == 0)
		    return c;				/* pass the buck */
		goto replot;				/* else fall into replot */

#ifdef	ROTATE
	    case 'R': 					/* rot-13 the text */
		Rotate = !Rotate;			/* toggle */
							/* and fall into re-plot */
#endif	ROTATE
	replot: 					/* nasty goto label */
	    case 'r': 
	    case '\014': 				/* control-L too */
		dispbuf.optr = dispbuf.olim = 0;
		dispbuf.outcount = pagecnt[pagesout];
		where = dispbuf.d_head.addr + dispbuf.outcount;
		x (lseek (io -> fidtxt, where, 0) < 0, "dspnote: bad seek");
		break;					/* are all set now */


	    default: 					/* pass the buck */
		return c;
	}
    } while (dispbuf.outcount < dispbuf.d_head.textlen);

    return c;						/* didn't field a command */
}
/*
 *	showit
 *
 *	accepts a pointer to a dsply_f and dumps text until runs out
 *	or the screen is filled.
 *	Counts things like lines, columns, and also prints a "more" line
 *
 *	Ray Essick	June 15, 1982
 */

showit (io, dbuf, Rotate)
struct io_f *io;
struct dsply_f *dbuf;
int     Rotate;
{
    int     lines,
            wides;					/* screen fill stuff */
    int     c;

    lines = 4;						/* header eats 4 */
    wides = 0;						/* start in col 1 */
    while (lines < nrows - 1 && dbuf -> outcount < dbuf -> d_head.textlen)
    {
	if (intflag)					/* user abort? */
	{
	    intflag = 0;
	    return (-1);				/* back to key processing */
	}
	if (dbuf -> optr == dbuf -> olim)		/* buffer is empty */
	{
	    x ((dbuf -> olim = read (io -> fidtxt, dbuf -> d_buf.txtbuf, BUFSIZE)) < 0, "dspnote: text read");
	    dbuf -> optr = 0;
	}
#ifdef	ROTATE
	if (Rotate)
	{
	    register char   cc;
	    cc = dbuf -> d_buf.txtbuf[dbuf -> optr];
	    if ((cc >= 'a' && cc <= 'm') || (cc >= 'A' && cc <= 'M'))
		cc += ROTATE;
	    else
		if ((cc >= 'n' && cc <= 'z') || (cc >= 'N' && cc <= 'Z'))
		    cc -= ROTATE;
	    putc (cc, stdout);
	}
	else
#endif	ROTATE
	{
	    putc (dbuf -> d_buf.txtbuf[dbuf -> optr], stdout);
	}
							/* show the character */
	dbuf -> outcount++;
	switch (dbuf -> d_buf.txtbuf[dbuf -> optr++])	/* some special chars */
	{
	    case '\n': 					/* next line */
		if (isinput ())				/* PLATO-brand erase-abort */
		    goto exisho;
		wides = 0;
		lines++;
		break;

	    case '\014': 				/* force next page */
		lines = nrows;				/* forces loop exit */
		break;

	    case '\t': 					/* almost forgot tabs */
		wides += (8 - (wides % 8));		/* tab stops */
		break;

	    case '\b': 					/* perverts using backspaces */
		wides--;
		break;

	    default: 					/* dull characters */
		wides++;
		break;
	}
	if (wides >= ncols)				/* test line overflow */
	{
	    lines++;
	    wides = 0;
	}
    }
exisho: 						/* typeahead exit */
    if (dbuf -> outcount < dbuf -> d_head.textlen)
    {
	at (0, 60);
	printf (Continued,
		(int) (dbuf -> outcount * 100L / (long) dbuf -> d_head.textlen));
    }
    at (0, 1);						/* grab command */
#ifdef PROMPT
    printf ("%s", PROMPT);
#endif
    c = gchar ();					/* grab command */
    printf ("\b \b");
    return c;						/* so let caller handle it */
}
