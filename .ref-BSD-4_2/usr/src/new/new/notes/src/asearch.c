static char *sccsid = "%W%";

/*
 *	asearch(io, notenum, respnum, grabname)
 *	int *notenum, *respnum; struct io_f *io;
 *
 *	searches for an article by the specified author. The author to
 *	look for is kept in io->xasys and io->xaname
 *	The search starts with note # notenum, and the respnum'th
 *	response of that note.
 *
 *	The search proceeds out to the end of the response chain and
 *	then goes through the previous note and its responses
 *	--so it is not strictly backwards, but instead is backwards on
 *	the notes and forward within a notes responses. 
 *	---This could be changed later.
 *
 *	Returns:	0 nothing found
 *			>0 Found something. Correct place is in
 *			   notenum and respnum....
 *			-1 did not specify a search string!
 *
 *
 *	Ray Essick			Feb 1982
 */

#include "parms.h"
#include "structs.h" 

asearch(io, notenum, respnum, grabname)
struct io_f *io;
int *notenum, *respnum;
{
	struct note_f   note;
	struct resp_f   rsprec;
	int     rblock, i, roffset;
	char    buf[SYSSZ + NAMESZ + 5];
	char    asys[SYSSZ + NAMESZ + 2];		/* must hold either ! */
	char    aname[NAMESZ];

	if (grabname || (io->xaname[0] == '\0')) {
		prompt("Search author: ");
		i = gline(buf, NNLEN + SYSSZ + 1);		/* grab name */
		if (i == 1)
			return(-1);				/* no name */
		i = sscanf(buf, "%[^!]!%s", asys, aname);
		if (i == 1) {
			for (i = 0; i < NAMESZ; i++)
				/* only said name */
				io->xaname[i] = asys[i];
			/* set up default system */
			strmove(SYSTEM, io->xasys);
		} else if (i == 2) {
			for (i = 0; i < SYSSZ; i++) {
				io->xasys[i] = asys[i];
			}
			for (i = 0; i < NAMESZ; i++) {
				io->xaname[i] = aname[i];
			}
		} else {
			warn("Bad author - retry");
			return(-1);
		}
	}

	if (*notenum > io->descr.d_nnote) {		/* check boundaries */
		*respnum = 0;
		*notenum = io->descr.d_nnote;
	}

	if (*respnum != 0) {
		getnrec(io, *notenum, &note);
		goto inloop;
	}

	while (*notenum > 0) {
		getnrec (io, *notenum, &note);
		if (note.n_stat & DELETED) {
			(*notenum)--;
			continue;				/* dead note */
		}
		if ((strcmp (io->xasys, note.n_id.sys) == 0) &&
		    (strcmp (io->xaname, note.n_auth.aname) == 0))
			return(*notenum);			/* found him! */
		*respnum = 1;					/* set it now */
inloop: 					/* for starting at a response */
		for ( ; *respnum <= note.n_nresp; (*respnum)++) {
			if (lrsp(io, *notenum, *respnum, &rsprec, &roffset,
			    &rblock) == -1)
				break;
			if ((strcmp(io->xasys, rsprec.r_id[roffset].sys) == 0)
			    && (strcmp(io->xaname, rsprec.r_auth[roffset].aname) == 0))
				return(*notenum);		/* found him */
		}
		*respnum = 0;			/* make it a main note */
		(*notenum)--;			/* and proceed to next note */
	}
	if (strcmp (SYSTEM, io->xasys) != 0)
		warn("Can't find any articles by `%s!%s'", io->xasys,
		    io->xaname);
	else 			/* local - don't echo system */
		warn("Can't find any articles by `%s'", io->xaname);
	return(0);
}
