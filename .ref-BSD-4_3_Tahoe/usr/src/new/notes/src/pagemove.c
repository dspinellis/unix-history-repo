#include "parms.h"
#include "structs.h"

#ifdef	RCSIDENT
static char rcsid[] = "$Header: pagemove.c,v 1.7.0.2 85/05/20 09:55:01 notes Rel $";
#endif	RCSIDENT

/*
 *	pagemove(io,where,io2,where2,lockflag)
 *
 *	Take the text pointed to by the (io,where) pair and stuff
 *	it into the (io2) notesfile. Return the address in
 *	(where2).
 *
 *	Lock the io2 notesfile for the transfer if the lockflag
 *	parameter is non-zero.
 *
 *	Ray Essick, March 1985
 */

pagemove (io, where, io2, where2, lockflag)
struct io_f *io;
struct daddr_f *where;
struct io_f *io2;
struct daddr_f *where2;
int     lockflag;
{
    char    buf[BUFSIZ];				/* for moving chunks */
    register int    bufchars;				/* in buffer */
    register long   moved;				/* actually moved */
    register long   total;
    register long   need;
    struct daddr_f  nwhere;

    if (where -> addr == 0 || where -> textlen == 0)
    {							/* no text or empty */
	where2 -> addr = 0;
	where2 -> textlen = 0;
	return;						/* cheap move */
    }

    if (lockflag)					/* lock if wanted */
	locknf (io2, TXTLOCK);

    /* 
     * position the input and output files.
     * we have to grab the free pointer for the output file.
     */
    x (lseek (io -> fidtxt, where -> addr, 0) != where -> addr, "pagemove: old seek");
    x (lseek (io2 -> fidtxt, 0L, 0) != 0, "pagemove: io2 seek 0");
    x (read (io2 -> fidtxt, where2, sizeof *where) != sizeof *where, "pagemove: read free");
    x (lseek (io2 -> fidtxt, where2 -> addr, 0) != where2 -> addr, "pagemove: new seek");
    moved = 0;
    total = where -> textlen;				/* total to move */
    bufchars = 0;
    where2 -> textlen = 0;				/* start empty */

    while (moved != total)				/* more text */
    {
	need = total - moved;				/* how much wanted? */
	if (need >= BUFSIZ)
	    need = BUFSIZ;				/* only so many at once */
	bufchars = read (io -> fidtxt, buf, ((int) need));/* read them */
	x (bufchars != need, "pagemove: bad read from old");/* check */
	x (write (io2 -> fidtxt, buf, bufchars) != bufchars, "pagemove: bad write to new");
	moved += bufchars;				/* bump counts */
	where2 -> textlen += bufchars;
    }
    x (where -> textlen != where2 -> textlen, "pagemove: moved wrong count");
    /* 
     * we now have shoved the text into the second notesfile.
     * We need to go back and update the free pointer.
     * and then we can return...
     */
    nwhere.textlen = 0;
    nwhere.addr = where2 -> addr + where2 -> textlen;
    if (nwhere.addr & 1)				/* odd? */
	nwhere.addr++;					/* round up */
    x (lseek (io2 -> fidtxt, 0L, 0) != 0, "pagemove: bad reseek");
    x (write (io2 -> fidtxt, &nwhere, sizeof nwhere) != sizeof nwhere, "pagemove: bad write free pointer");

    if (lockflag)					/* cleanup */
	unlocknf (io2, TXTLOCK);
}
