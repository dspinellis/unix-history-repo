static char *sccsid = "@(#)find.c	1.1\t1/23/83";

#include "parms.h"
#include "structs.h"
/*
 *	chknote(io, noteid, note)
 *	see if a copy of the note specified by noteid is in the notefile
 *	returns the number of the note (0 if no note)
 *	NOTE: this routine is rather inefficient - since it 
 *	will go through the entire file to discover that the note is
 *	not in the notefile... This should be done over so that the
 *	access is run somewhat better/faster..
 *
 *	Original Coding:	Ray Essick	December 1981
 */

chknote (io, noteid, note)
struct io_f *io;
struct id_f *noteid;
struct note_f  *note;
{
    int     i;

    for (i = 1; i <= io->descr.d_nnote; i++) {
	getnrec(io, i, note);
	if (strcmp(noteid->sys, note->n_id.sys) != 0) {
	    continue;					/* mismatches systems */
	}
	if (noteid->uniqid == note->n_id.uniqid) {
	    return(i);
	}
    }
    return(0);						/* not found */
}

/*
 *	chkresp(io, respid, note, notenum)
 *	check the specified response to see if a response exists with
 *	the specified unique identifier
 *
 *	This too can be speeded up similarly to the chknote routine..
 *	but we shall worry about it later..after it already works.
 *
 *	Original Coding:	Ray Essick	December 1981
 */

chkresp (io, respid, note, notenum)
struct io_f *io;
struct id_f *respid;
struct note_f  *note;
{
    struct resp_f   rrec;
    int     roffset,
            rrecnum;
    int     i;

    for (i = 1; i <= note->n_nresp; i++) {
	if (lrsp(io, notenum, i, &rrec, &roffset, &rrecnum) == -1) {
	    continue;			/* no response */
	}
	if (strcmp(respid->sys, rrec.r_id[roffset].sys) != 0) {
	    continue;			/* mismatch systems */
	}
	if (respid->uniqid == rrec.r_id[roffset].uniqid) {
	    return(i);			/* return the response number */
	}
    }
    return(0);				/* is not a response to this note */
}
