static char *sccsid = "@(#)lrsp.c	1.1\t1/23/83";

#include "parms.h"
#include "structs.h"
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
    struct note_f   nrec;		/* note descriptor */
    int     prec;	/* which physical response group we is looking in */

    getnrec (io, notenum, &nrec);	/* get the note info */
    if (resp <= 0) {
	return(-1);			/* that was dumb */
    }
    if (resp > nrec.n_nresp) {
	return(-1);			/* this too was dumb of him */
    }
    prec = nrec.n_rindx;		/* record # of first response */
    *poffset = 0;

    getrrec (io, prec, rrec);		/* get the first response group */
    while (resp) {
	while (rrec->r_stat[*poffset] & DELETED) {
	    if (++*poffset == RESPSZ) {
		*poffset = 0;
		if ((prec = rrec->r_next) == -1) {
		    return(-1);				/* broken chain */
		}
		getrrec (io, prec, rrec);		/* passed this buffer */
	    }
	}
	if (--resp) {
	    rrec->r_stat[*poffset] |= DELETED;    /* this is the wrong one. lets ignore it. */
	}
/* we can do this because we never write it back out to screw up others */
    }
    *recnum = prec;	/* set up the response index for the return */
    return(0);
}
