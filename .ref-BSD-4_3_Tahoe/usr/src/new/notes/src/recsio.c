#ifdef	RCSIDENT
static char rcsid[] = "$Header: recsio.c,v 1.7 85/01/18 15:38:05 notes Rel $";
#endif	RCSIDENT

/*
 *  init(io,p), finish(io)  struct io_f *io, char *p
 *    initopens the three i/o files and initializes session stats
 *
 *    finish(io) closes all those files.
 *
 *  getnrec, putnrec, getrrec, putrrec
 *  getdscr, putdscr, gettrec, puttrec
 *  each gets or puts physical records inside its appropriate file.
 *
 */

#include "parms.h"
#include "structs.h"
#include <sys/types.h>
#include <sys/stat.h>

long    lseek ();					/* for sake of lint */

init (io, p) struct io_f   *io;
char   *p;
{
    int     i;						/* counter */
    struct auth_f   me;					/* identify self */

    if ((i = opennf (io, p)) < 0)			/* try to open */
    {
	return (i);					/* bad luck opening */
    }

    getdscr (io, &io -> descr);
    if (io -> descr.d_format != DBVERSION)		/* bad version */
    {
	printf ("%s: wrong database format (is %ld, want %ld)\n",
		io -> fullname, io -> descr.d_format, (long) DBVERSION);
	closenf (io);					/* close files */
	return (QUITBAD);
    }
    getname (&me, 0);					/* grab identity for permissions */
    getperms (io, 0, me.aname);				/* go establish access rights */

    io -> nrspwrit = io -> nnotwrit = 0;		/* set up stats */
    io -> nrspread = io -> nnotread = 0;
    io -> nnotxmit = io -> nrspxmit = 0;
    io -> nnotrcvd = io -> nrsprcvd = 0;
    io -> nnotdrop = io -> nrspdrop = 0;
    io -> norphans = io -> adopted = 0;
    io -> xstring[0] = io -> xauthor[0] = '\0';		/* clear search strings */

    time (&io -> entered);				/* get entry time */

    return (0);						/* all set */
}

/*
 *	Open a notesfile.
 *
 *	given a name, pick the appropriate notesfile. This includes
 *	searching along "search paths" once we get that implemented.
 *	Absolute path names are permitted.
 */

opennf (io, p)
struct io_f *io;
char   *p;
{
    char    fn[WDLEN];
    char   *q,
           *r,
           *s;
    char   *endname;
    int     i;
    struct stat statbuf;

    if (p != (char *) NULL)
    {							/* newly-opened */
	if (*p == '/')					/* explicit path */
	{
	    q = rindex (p, '/');			/* find last '/' */
	    for (r = p, s = io -> basedir; r < q;)	/* copy directory */
		*s++ = *r++;
	    *s++ = '\0';				/* terminate */
	    endname = ++q;
	}
	else
	{
/*
 *	This is where we should start looking for the
 *	notesfile along a search path.
 */
	    strcpy (io -> basedir, Mstdir);		/* default dir */
	    endname = p;				/* for errors */
	}

	if (chkpath (endname))
	{
	    printf ("Invalid notefile name: '%s'\n", p);
	    return (QUITBAD);
	}
	q = endname;
	r = io -> nf;
	i = NNLEN;
	while ((*r++ = *q++) && --i);			/* notesfile name */

	sprintf (fn, "%s/%s", io -> basedir, endname);	/* open the directory */
	if (stat (fn, &statbuf) != 0)			/* see if directory */
	{
	    printf ("No such notesfile: '%s'\n", p);
	    return (QUITNEX);
	}
    }

    sprintf (io -> fullname, "%s/%s", io -> basedir, io -> nf);

    sprintf (fn, "%s/%s", io -> fullname, TEXT);
    if ((io -> fidtxt = open (fn, 2)) < 0)
    {
	return (QUITBAD);				/* bad nf */
    }

    sprintf (fn, "%s/%s", io -> fullname, INDEXN);
    if ((io -> fidndx = open (fn, 2)) < 0)
    {
	close (io -> fidtxt);
	return (QUITBAD);
    }

    sprintf (fn, "%s/%s", io -> fullname, INDEXR);
    if ((io -> fidrdx = open (fn, 2)) < 0)
    {
	close (io -> fidtxt);
	close (io -> fidndx);
	return (QUITBAD);				/* bad nf */
    }

    return 0;						/* all's well */
}


finish (io)
struct io_f *io;
{
    long    left;
    struct when_f   lvtime;				/* for days used */

#ifdef	STATS						/* if keeping statistics */
    locknf (io, DSCRLOCK);				/* update statistics */
    getdscr (io, &io -> descr);
    io -> descr.d_notwrit += io -> nnotwrit;
    io -> descr.d_rspwrit += io -> nrspwrit;
    io -> descr.d_notread += io -> nnotread;
    io -> descr.d_rspread += io -> nrspread;
    io -> descr.d_notxmit += io -> nnotxmit;
    io -> descr.d_rspxmit += io -> nrspxmit;
    io -> descr.d_notrcvd += io -> nnotrcvd;
    io -> descr.d_rsprcvd += io -> nrsprcvd;
    io -> descr.d_notdrop += io -> nnotdrop;
    io -> descr.d_rspdrop += io -> nrspdrop;
    io -> descr.d_orphans += io -> norphans;
    io -> descr.d_adopted += io -> adopted;
    io -> descr.entries++;				/* count of entries */
    time (&left);
    io -> descr.walltime += left - io -> entered;	/* time spent in nf */
    gettime (&lvtime);
    if ((lvtime.w_day != io -> descr.d_lastuse.w_day) ||
	    (lvtime.w_month != io -> descr.d_lastuse.w_month) ||
	    (lvtime.w_year != io -> descr.d_lastuse.w_year))
    {
	io -> descr.d_daysused++;
	copydate (&lvtime, &io -> descr.d_lastuse);
    }
    putdscr (io, &io -> descr);				/* update the block */
    unlocknf (io, DSCRLOCK);
#endif	STATS						/* end of stats gathering */

    closenf (io);
}

closenf (io)
struct io_f *io;
{

    x (close (io -> fidtxt) < 0, "finish: text fail");
    x (close (io -> fidndx) < 0, "finish: nindx fail");
    x (close (io -> fidrdx) < 0, "finish: rindx fail");
}



getnrec (io, n, note) struct note_f *note;		/* n is the number of the note to get.  0 is policy note */
struct io_f *io;
{
    long    where;					/* going to seek here eventually */
    struct descr_f *descr;				/* for sizeof below */

    x (n < 0, "getnrec: negative recnum");
    where = sizeof (*descr) + n * sizeof (*note);
    x (lseek (io -> fidndx, where, 0) < 0, "getnrec: seek");
    x (read (io -> fidndx, note, sizeof *note) < sizeof *note, "getnrec: read");
}

putnrec (io, n, note) struct note_f *note;		/* n is the number of the note to put.  0 is policy note */
struct io_f *io;
{
    long    where;					/* going to seek here eventually */
    struct descr_f *descr;				/* for sizeof below */

    x (n < 0, "putnrec: negative recnum");
    where = sizeof (*descr) + n * sizeof (*note);
    x (lseek (io -> fidndx, where, 0) < 0, "putnrec: seek");
    x (write (io -> fidndx, note, sizeof *note) < sizeof *note, "putnrec: write ");
}

getdscr (io, descr) struct descr_f *descr;
struct io_f *io;
{

    x (lseek (io -> fidndx, 0L, 0) < 0, "getdscr: seek");
    x (read (io -> fidndx, descr, sizeof *descr) < sizeof *descr, "getdscr: read");
}

putdscr (io, descr) struct descr_f *descr;
struct io_f *io;
{

    x (lseek (io -> fidndx, 0L, 0) < 0, "putdscr: seek");
    x (write (io -> fidndx, descr, sizeof *descr) < sizeof *descr, "putdscr: write");
}

getrrec (io, n, resp) struct resp_f *resp;		/* n is the number of the resp to get */
struct io_f *io;
{
    long    where;					/* going to seek here eventually */
    int     a;						/* size of free link */
    x (n < 0, "getrrec: negative recnum");

    where = sizeof a + n * sizeof (*resp);
    x (lseek (io -> fidrdx, where, 0) < 0, "getrrec: seek");
    x (read (io -> fidrdx, resp, sizeof *resp) < sizeof *resp, "getrrec: read");
}

putrrec (io, n, resp) struct resp_f *resp;		/* n is the number of the resp to put */
struct io_f *io;
{
    long    where;					/* going to seek here eventually */
    int     a;						/* size of free link */
    x (n < 0, "putrrec: negative recnum");

    where = sizeof a + n * sizeof (*resp);
    x (lseek (io -> fidrdx, where, 0) < 0, "putrrec: seek");
    x (write (io -> fidrdx, resp, sizeof *resp) < sizeof *resp, "putrrec: write");
}

/*
 *	puttrec(i&io_f, &FILE, &daddr_f, long)
 *
 *	reads cound characters from the input stream specified ad
 *	puts them into the text file. The address is returned...
 *
 *	Almost identical to the code in "pagein.c" and should 
 *	probably be the same code with the third parameter being
 *	the count and meaning "until EOF" if -1 or something..
 *
 *	Ray Essick	May 8, 1982
 */
long    puttrec (io, zfile, where, count)
struct io_f *io;
FILE * zfile;
struct daddr_f *where;
long    count;
{

    int     i;
    long    nchars;
    long    ignored;
    int     ignoring;
    struct daddr_f  nwhere;
    struct txtbuf_f buf;				/* hold bunches of text */

    if (count == 0)					/* empty text */
    {
	where -> addr = 0;
	where -> textlen = 0;				/* standard empty */
	return ((long) 0);
    }

    locknf (io, TXTLOCK);				/* grab access to the file */
    x (lseek (io -> fidtxt, 0L, 0) < 0, "puttrec: bad seek 0");
    x (read (io -> fidtxt, where, sizeof nwhere) < 0, "puttrec: read 0");
    x (lseek (io -> fidtxt, where -> addr, 0) < 0, "puttrec:badseek");

    nchars = 0;
    ignored = 0;
    ignoring = 0;
    i = 0;
    while ((nchars + ignored) != count)			/* grab input */
    {
	if (!ignoring)
	{
	    if (i == BUFSIZE)				/* flush full buffer */
	    {
		x (write (io -> fidtxt, buf.txtbuf, BUFSIZE) != BUFSIZE,
			"puttrec: bad text");
		i = 0;					/* reset buffer */
	    }
	    buf.txtbuf[i++] = getc (zfile);
	    if (++nchars >= io -> descr.d_longnote)	/* gotta truncate */
		ignoring++;				/* start now */
	}
	else
	{
	    (void) getc (zfile);			/* punt */
	    ignored++;
	}
    }
    if (i)						/* write partial buf */
	x (write (io -> fidtxt, buf.txtbuf, i) != i, "puttrec: bad text");
    if (ignored)					/* write warning */
    {
	sprintf (buf.txtbuf, "\n\n%s ignored %ld excess bytes\n",
		System, ignored);
	i = strlen (buf.txtbuf);			/* get length */
	x (write (io -> fidtxt, buf.txtbuf, i) != i, "puttrec: bad text");
	nchars += i;					/* count extras */
    }
/*
 * fix count of characters sucked in daddr_f structure
 */
    where -> textlen = nchars;				/* fill header */
/*
 *	now fix the free pointer
 */
    x (lseek (io -> fidtxt, 0L, 0) < 0, "puttrec:bad reseek");
    nwhere.addr = where -> addr + nchars;
    if (nwhere.addr & 1)				/* odd ? */
	nwhere.addr++;					/* round to word boundary */
    x (write (io -> fidtxt, &nwhere, sizeof nwhere) != sizeof nwhere, "puttrec: badupdate");

    unlocknf (io, TXTLOCK);
    return ((long) nchars);
}
