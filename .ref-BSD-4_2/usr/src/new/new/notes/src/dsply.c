static char *sccsid = "%W%";

#include "parms.h"
#include "structs.h"

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


dspnote(io, note, notenum)
struct io_f *io;
struct note_f  *note;
{
	int
	    c;
	long where;					/* for seeking */
	struct txthead_f    txthead;			/* used for seeking */
	struct dsply_f  dispbuf;
	long pagecnt[PAGESAV + 1];
	int     pagesout;


	io->nnotread++;				/* bump count of notes read */
	pagesout = 0;
	pagecnt[pagesout] = 0;

	if (note->n_addr.addr == 0)
		dispbuf.d_head.textlen = 0;	/* so can get a header */
	else
		gethrec (io, &note->n_addr, &dispbuf.d_head); /* get header */

	dispbuf.outcount = dispbuf.optr = dispbuf.olim = 0; /* fix buffer */
	do {					/* always show the first page */
		/* display loop */
		erase();			/* write header crap */
		at (1, 1);
		if (notenum != 0) {
			if (dispbuf.outcount)			/* nth page */
				printf ("[Continued] ");
			printf ("Note %d", notenum);
		}
		center (io->descr.d_title, NNLEN, 1, 40 - NNLEN / 2);
		if (note->n_nresp > 0) {
			at (1, 67);
			printf ("%d response", note->n_nresp);
			if (note->n_nresp > 1)
				printf ("s");
		}
		at (2, 1);
		printf ("%s", note->n_auth.aname);
		center (note->ntitle, TITLEN, 2, 40 - TITLEN / 2);
		at (2, 59);
		prdate (&note->n_date);
		if (strcmp (SYSTEM, note->n_id.sys) &&
		    strcmp (Anon, note->n_auth.aname)) {
			at (3, 1);
			/* print system name if not local */
			printf ("(at %s)", note->n_id.sys);
		}
		if (note->n_stat & DIRMES)
			center (io->descr.d_drmes, DMLEN, 3, 40 - DMLEN / 2);
		else {
			if (note->n_stat & WRITONLY) {
				at (3, 31);
				printf ("-- (Write Only) --");
			}
		}
		if (note->n_stat & FRMNEWS) {
			at (3, 59);
			printf ("%s", Fromnews);
		}
		if (dispbuf.d_head.textlen == 0) {
			c = (-1);
			break;		/* header is it for empties */
		}
		at (4, 1);
		putc ('\n', stdout);		/* make sure soft-tabs work */
		c = showit (io, &dispbuf);
		switch (c) {
		case ' ': 			/* wants the next page */
		case 'l':			/* universal seq, RLS */
			if (pagesout < PAGESAV)
				pagecnt[++pagesout] = dispbuf.outcount;
			/* save new start */
			break;

		case '-': 			/* go back to previous */
		case '\b': 			/* same for backspace */
			if (pagesout-- == 0)
				return(c);		/* pass the buck */
			dispbuf.optr = dispbuf.olim = 0;
			dispbuf.outcount = pagecnt[pagesout];
			where = note->n_addr.addr + (sizeof txthead) + dispbuf.outcount;
			x (lseek (io->fidtxt, where, 0) < 0, "dspnote: bad seek");
			break;				/* are all set now */
		
		case '\014':			/* redraw current screen */
			dispbuf.optr = dispbuf.olim = 0;
			dispbuf.outcount = pagecnt[pagesout];
			where = note->n_addr.addr + (sizeof txthead) + dispbuf.outcount;
			x (lseek (io->fidtxt, where, 0) < 0, "dspnote: bad seek");
			break;				/* are all set now */


		default: 				/* pass the buck */
			return(c);
		}
	} while (dispbuf.outcount < dispbuf.d_head.textlen);

	if (note->n_stat & CONTINUED) {
		at (0, -40);
		printf ("-- Continued in next response --");
	}
	return(c);				/* didn't field a command */
}

/*
 *	Code to print the header and manage the paging of a response
 *	It calls the "showit" routine to print pages of text
 */
dspresp(io, note, resp, phys, logical, notenum)
struct io_f *io;
struct note_f  *note;
struct resp_f  *resp;
{
	int     c;
	struct txthead_f    txthead;
	struct dsply_f  dispbuf;

	long where,
	pagecnt[PAGESAV];		/* stack for backing up */
	int     pagesout;

	io->nrspread++;			/* bump count or responses read */
	pagesout = 0;
	pagecnt[pagesout] = 0;

	if (resp->r_addr[phys].addr == 0)
		dispbuf.d_head.textlen = 0;	/* so can get a header */
	else
		gethrec (io, &resp->r_addr[phys], &dispbuf.d_head);
	/* get header */

	dispbuf.outcount = dispbuf.optr = dispbuf.olim = 0; /* fix buffer */
	do {					/* always make 1 pass */
		erase ();			/* paint the header */
		at (1, 1);
		if (dispbuf.outcount)		/* nth page */
			printf ("[Continued] ");
		printf ("Note %d", notenum);
		center (io->descr.d_title, NNLEN, 1, 40 - NNLEN / 2);
		at (2, 1);
		printf ("%s", resp->r_auth[phys].aname);
		at (2, 31);
		printf ("Response %2d of %2d", logical, note->n_nresp);
		at (2, 59);
		prdate (&resp->r_when[phys]);
		if (strcmp (SYSTEM, resp->r_id[phys].sys) != 0 &&
		    strcmp (Anon, resp->r_auth[phys].aname) != 0) {
			/* print sys name if not here */
			at (3, 1);
			printf ("(at %s)", resp->r_id[phys].sys);
		}
		if (resp->r_stat[phys] & DIRMES)
			center (io->descr.d_drmes, DMLEN, 3, 40 - DMLEN / 2);
		if (resp->r_stat[phys] & FRMNEWS) {
			at (3, 59);
			printf ("%s", Fromnews);
		}
		if (dispbuf.d_head.textlen == 0) {
			c = (-1);
			break;			/* header is all for empties */
		}
		at (4, 1);
		putc ('\n', stdout);		/* make sure soft-tabs work */
		c = showit (io, &dispbuf);
		switch (c) {
		case ' ':			/* wants the next page */
		case 'l':			/* universal seq, RLS */
			if (pagesout < PAGESAV)
				pagecnt[++pagesout] = dispbuf.outcount;
			/* save new start */
			break;

		case '-': 			/* go back to previous */
		case '\b':
			if (pagesout-- == 0)
				return c;		/* pass the buck */
			dispbuf.optr = dispbuf.olim = 0;
			dispbuf.outcount = pagecnt[pagesout];
			where = note->n_addr.addr + (sizeof txthead) + dispbuf.outcount;
			x (lseek (io->fidtxt, where, 0) < 0, "dspnote: bad seek");
			break;				/* are all set now */

		case '\014':			/* redraw current screen */
			dispbuf.optr = dispbuf.olim = 0;
			dispbuf.outcount = pagecnt[pagesout];
			where = note->n_addr.addr + (sizeof txthead) + dispbuf.outcount;
			x (lseek (io->fidtxt, where, 0) < 0, "dspnote: bad seek");
			break;				/* are all set now */

		default: 				/* pass the buck */
			return c;
		}
	} 
	while (dispbuf.outcount < dispbuf.d_head.textlen);
	if (resp->r_stat[phys] & CONTINUED) {
		at (0, -40);
		printf ("-- Continued in next response --");
	}
	return c;				/* didn't field a command */
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
showit(io, dbuf)
struct io_f *io;
struct dsply_f *dbuf;
{
	int     lines, wides;			/* screen fill stuff */
	int     c;
	long    retval = 0;

	lines = 4;				/* header eats 4 */
	wides = 0;				/* start in col 1 */
	while (lines < nrows-2 && dbuf->outcount < dbuf->d_head.textlen) {
		if (dbuf->optr == dbuf->olim) {		/* buffer is empty */
			x ((dbuf->olim = read(io->fidtxt, dbuf->d_buf.txtbuf,
			    BUFSIZE)) < 0, "dspnote: text read");
			dbuf->optr = 0;
		}
		/* show the character */
		putchar(dbuf->d_buf.txtbuf[dbuf->optr]);
		dbuf->outcount++;
		switch (dbuf->d_buf.txtbuf[dbuf->optr++]) {
		/* some special chars */
		case '\n': 				/* next line */
			wides = 0;
			lines++;
			/* check for typeahead every once in a while */
			if (isinput())
				goto exisho;
			break;

		case '\014': 				/* force next page */
			lines = nrows;			/* forces loop exit */
			break;

		case '\t': 				/* almost forgot tabs */
			wides += (8 - (wides % 8));	/* tab stops */
			break;

		case '\b':			/* perverts using backspaces */
			wides--;
			break;

		default: 			/* dull characters */
			wides++;
			break;
		}
		if (wides >= ncols) {		/* test line overflow */
			lines++;
			wides = 0;
		}
	}
	if (dbuf->outcount < dbuf->d_head.textlen) {
		at (-1, 60);
		printf (Continued, dbuf->outcount * 100 / dbuf->d_head.textlen);
	}

exisho:
	cmdprompt();
	c = gchar();				/* grab command */
	return(c);				/* so let caller handle it */
}
