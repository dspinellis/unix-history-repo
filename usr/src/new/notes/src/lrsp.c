#include "parms.h"
#include "structs.h"

#ifdef	RCSIDENT
static char rcsid[] = "$Header: lrsp.c,v 1.7 85/01/18 15:16:05 notes Rel $";
#endif	RCSIDENT

/*
 *	this routine takes in a note number, the io descriptors and
 *	and a LOGICAL response number to look for. 
 *	It the returns the physical response index locations.
 *	It returns the response index record, and the offset within
 *	that record for the correct response
 *
 *	Implemented the easy/cheap way. We just scan along the response
 *	chain, counting up the undeleted responses.
 *	Once we find the correct number, we return with the current
 *	set of pointers. What could be easier?
 *	In the event that we hit the end of responses before we find
 *	the response we want (e.g. the dummy wanted a nonexistent response)
 *	we return the index of the last response.
 *
 *	Returns:	0 if all goes well
 *			-1 if bad logical response number 
 *
 *	Ray Essick	May 22, 1981.
 *
 */

lrsp (io, notenum, resp, rrec, poffset, recnum)
struct io_f *io;
struct resp_f  *rrec;
int    *recnum,
       *poffset;
{
    struct note_f   nrec;				/* note descriptor */
    int     prec;					/* which physical response group we is looking in */

    getnrec (io, notenum, &nrec);			/* get the note info */
    if (resp <= 0)
	return (-1);					/* that was dumb */
    if (resp > nrec.n_nresp)
	return (-1);					/* this too was dumb of him */
    prec = nrec.n_rindx;				/* record # of first response */
    *poffset = 0;

    getrrec (io, prec, rrec);				/* first resp block */
    while (resp > rrec -> r_last)			/* hi-speed along */
    {
	if ((prec = rrec -> r_next) == -1)
	    return (-1);				/* broken chain */
	getrrec (io, prec, rrec);			/* passed this buffer */
    }
    /* 
     *	We should now be in the block that contains the response
     */
    {
	register int    counted;
	counted = (-1);					/* active in this block */
	*poffset = 0;					/* start of block */
	while (1)					/* forever */
	{
	    while (rrec -> r_stat[*poffset] & DELETED)	/* skip holes */
	    {
		++*poffset;				/* and try next */
		x (*poffset == RESPSZ, "lrsp: r_last lied!");
	    }
	    counted++;					/* a live one */
	    if (rrec -> r_first + counted == resp)	/* got it */
	    {
		break;					/* get out */
	    }
	    ++*poffset;					/* on to next */
	    /* 
	     * this above is legit since we KNOW the resp is in this
	     * block so we'll never go over the end
	     */
	}
    }
    *recnum = prec;					/* set up return */
    return 0;
}
