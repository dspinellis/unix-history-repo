#include "parms.h"
#include "structs.h"

#ifdef	RCSIDENT
static char rcsid[] = "$Header: lprnote.c,v 1.7.0.3 85/03/19 19:39:05 notes Rel $";
#endif	RCSIDENT

/*
 *	lprnote(io, lprfile, toc, notenum, note, justtitle)
 *
 *	prints the specified note to lprfile, and makes an entry in
 *	toc file. Paging is taken care of and the page/line stuff is
 *	correctly set at the conclusion.
 *
 *	lprresp(io, lprfile, toc, respnum, rsprec, roffset)
 *
 *	Same as the lprnote routine. Works for responses though.
 *
 *	pagebreak(lprfile)
 *
 *	forces a page break.
 *
 *	Ray Essick	May 13, 1982
 */

#define	NOTENEED	7
#define	RESPNEED	7

extern int  length,					/* length of page */
            left,					/* left on the page */
            page;					/* the pae we are on */

lprnote (io, lprfile, toc, notenum, note, justtitle)
struct io_f *io;
FILE * lprfile, *toc;
struct note_f  *note;
{
    struct resp_f   rsprec;
    int     roffset,
            rblock;
    char    line[CMDLEN];
    int     i;
    char    ztime[DATELEN];

    if (left < NOTENEED)				/* room for header and some text? */
	pagebreak (lprfile);				/* make it that way */

    if (strcmp (note -> n_auth.asystem, Authsystem) &&
	    strcmp ("Anonymous", note -> n_auth.aname))
    {
#ifdef	USERHOST
	sprintf (line, "%s@%s", note -> n_auth.aname, note -> n_auth.asystem);
#else
	sprintf (line, "%s!%s", note -> n_auth.asystem, note -> n_auth.aname);
#endif	USERHOST
    }
    else
	sprintf (line, "%s", note -> n_auth.aname);
    sprdate (&note -> n_date, ztime);			/* format date */

    fprintf (toc, "%3d%s ", notenum, note -> n_stat & DIRMES ? "(*)" : "   ");
    fprintf (toc, "%-*s", TITLEN, note -> ntitle);
    fprintf (toc, "%-*s  %-*s", SYSSZ + NAMESZ + 2, line, DATELEN, ztime);
    if (!justtitle)					/* no page for titles only */
	fprintf (toc, "    %d", page);
    putc ('\n', toc);					/* end the line */

/*
 *	only proceed if we are dumping text. Otherwise we stop with
 *	just the table of contents entry
 */

    if (justtitle)					/* no text output */
	return;

    fprintf (lprfile, "\n====    ====    ====    ====    ====    ====    ====\n");
    fprintf (lprfile, "Note %-3d %s  ", notenum,
	    note -> n_stat & DIRMES ? "(*)" : "   ");
    fprintf (lprfile, "%-*s", TITLEN, note -> ntitle);	/* title */

    if (note -> n_nresp)
	fprintf (lprfile, "   %d response%c", note -> n_nresp,
		note -> n_nresp > 1 ? 's' : ' ');
    putc ('\n', lprfile);

    fprintf (lprfile, "%-*s", SYSSZ + NAMESZ + 1, line);

    fprintf (lprfile, "        %s\n\n", ztime);

    left -= 5;						/* count off the header lines */

    left -= pageout (io, &note -> n_addr, lprfile);	/* dump text */

    while (left < 0)
    {
	page++;
	left += length;
    }


    for (i = 1; i <= note -> n_nresp; i++)		/* dump responses */
    {
	if (lrsp (io, notenum, i, &rsprec, &roffset, &rblock) == -1)
	    break;					/* bad chain */
	lprresp (io, lprfile, toc, notenum, i, &rsprec, roffset);
    }
}

/*
 *
 *	dump a response to a "pr" stream.
 */

lprresp (io, lprfile, toc, notenum, respnum, rsprec, phys)
struct io_f *io;
FILE * lprfile,
*toc;
struct resp_f  *rsprec;
{
    char    ztime[DATELEN];
    char    line[CMDLEN];

    if (left < RESPNEED)				/* room on page? */
	pagebreak (lprfile);

    if (strcmp (rsprec -> r_auth[phys].asystem, Authsystem) &&
	    strcmp ("Anonymous", rsprec -> r_auth[phys].aname))
    {
#ifdef	USERHOST
	sprintf (line, "%s@%s", rsprec -> r_auth[phys].aname,
		rsprec -> r_auth[phys].asystem);
#else
	sprintf (line, "%s!%s", rsprec -> r_auth[phys].asystem,
		rsprec -> r_auth[phys].aname);
#endif	USERHOST
    }
    else
	sprintf (line, "%s", rsprec -> r_auth[phys].aname);
    sprdate (&rsprec -> r_when[phys], ztime);
    fprintf (lprfile, "\n==== ==== ====\n");
    fprintf (lprfile, "Response %-4d to Note %-4d\n\t%-*s  %-*s\n\n",
	    respnum, notenum, SYSSZ + NAMESZ + 2, line, DATELEN, ztime);

    left -= 5;						/* count the header */

    left -= pageout (io, &rsprec -> r_addr[phys], lprfile);

    while (left < 0)
    {
	page++;
	left += length;
    }
}

/*
 *	force a page break
 */

pagebreak (zfile)
FILE * zfile;
{
    putc ('\f', zfile);					/* form feed */
    page++;
    left = length;
}
