#include "parms.h"
#include "structs.h"

#ifdef	RCSIDENT
static char rcsid[] = "$Header: pagein.c,v 1.7 85/01/18 15:33:36 notes Rel $";
#endif	RCSIDENT

/*
 *	pagein(zfile, where)
 *	FILE *zfile; struct daddr_f *where;
 *
 *	reads in a single 'page' as defined by the notesfile system and
 *	returns the count of characters moved.
 *
 *	Original Coding:	Ray Essick	Long agao
 *		moved out of gtext.c and mult.c and others   4/5/82
 *	Converted for infinite size notes	RBE 5/8/82
 *
 */

long    pagein (io, zfile, where)
struct io_f *io;
FILE * zfile;
struct daddr_f *where;
{

    register int    c;
    register int    i;
    register long   nchars;
    register long   ignored;				/* number ignored */
    register int    ignoring;				/* whether ignoring */
    struct daddr_f  nwhere;
    struct txtbuf_f buf;				/* hold text */

    locknf (io, TXTLOCK);				/* CRITICAL */
    x (lseek (io -> fidtxt, 0L, 0) < 0, "pagein: bad seek 0");
    x (read (io -> fidtxt, where, sizeof *where) < 0, "pagein: read 0");
    x (lseek (io -> fidtxt, where -> addr, 0) < 0,
	    "pagein: bad seek 1");

    where -> textlen = 0;				/* no text yet */
    nchars = 0;
    i = 0;
    ignoring = 0;					/* save em for now */
    ignored = 0;
    while ((c = getc (zfile)) != EOF)			/* grab input */
    {
	if (!ignoring)
	{
	    if (i == BUFSIZE)				/* flush full buffer */
	    {
		x (write (io -> fidtxt, buf.txtbuf, BUFSIZE) != BUFSIZE, "pagein: bad text");
		i = 0;					/* reset buffer */
	    }
	    buf.txtbuf[i++] = c;
	    if (++nchars >= io -> descr.d_longnote)	/* count characters */
	    {
		ignoring++;				/* don't add any more */
	    }
	}
	else						/* ignoring */
	{
	    ignored++;
	}
    }
    if (i != 0)						/* if we've got some */
	x (write (io -> fidtxt, buf.txtbuf, i) != i, "pagein: bad text");
    if (ignored)					/* write warning */
    {
	sprintf (buf.txtbuf, "\n\n%s ignored %ld excess bytes\n",
		System, ignored);
	i = strlen (buf.txtbuf);			/* get length */
	x (write (io -> fidtxt, buf.txtbuf, i) != i, "pagein: bad text");
	nchars += i;					/* count extras */
    }
/*
 *	fix count in the header
 */
    where -> textlen = nchars;				/* fill header */
/*
 *	fix free pointer
 */
    x (lseek (io -> fidtxt, 0l, 0) < 0, "pagein:bad reseek");
    nwhere.addr = where -> addr + nchars;
    if (nwhere.addr & 1)				/* odd?? */
	nwhere.addr++;					/* round to word boundary */
    x (write (io -> fidtxt, &nwhere, sizeof nwhere) != sizeof nwhere, "pagein: badupdate");

    unlocknf (io, TXTLOCK);
    return ((long) nchars);
}
