static char *sccsid = "@(#)tsearch.c	1.1\t1/23/83";

#include "parms.h"
#include "structs.h"
/*
 *	tsearch(io, fromnum, grabstring)
 *		search notetitles from note #fromnum back towards 1
 *	looking for the string in io->xstring. If that string is
 *	empty or grabstring is true, prompt the user for a string.
 *
 *	Returns:	0 if searched and not found
 *			>0 the note number which matched 
 *			-1 null string to search for.
 *
 *	Original coding:	Ray Essick	January 1982
 *	Modified:		Malcolm Slaney	July 1982
 *		to separate the gathering of a string and
 *		the actual search. A Good plan. (RBE)
 */

tsearch (io, fromnum, grabstring)
struct io_f *io;
{
    register int    i;

    if (grabstring || io->xstring[0] == '\0') {
	prompt("Search String: ");
	i = gline (io->xstring, TITLEN);		/* grab one */
	if (i == 1) {
	    io->xstring[0] = '\0';
	    return - 1;
	}
    }
    fromnum = findtitle (io, fromnum);
    if (fromnum > 0) {
	return (fromnum);
    } else {
	/* fix by RLS to clear line */
	warn("%s: Not Found", io->xstring);
    }
    return 0;
}

findtitle (io, fromnum)
struct io_f *io;
{
    struct note_f   note;				/* hold the note descriptor */
    register int    i,
                    j,
                    xlength;
    for (j = 0; io->xstring[j]; j++) {
	io->xstring[j] = tolcase (io->xstring[j]);
    }
    if (io->xstring[0] == '\0')	{	/* if still empty then */
	return(-1);			/* he doesn't really wanna search */
    }
    xlength = 0;
    i = 0;
    while (io->xstring[i++]) {
	xlength++;
    }
    if (fromnum > io->descr.d_nnote) {
	fromnum = io->descr.d_nnote;
    }

    while (fromnum > 0) {
	getnrec (io, fromnum, &note);			/* grab descriptor */
	if (note.n_stat & DELETED) {
	    fromnum--;
	    continue;					/* skip this one */
	}
	for (j = 0; j < TITLEN; j++) {
	    note.ntitle[j] = tolcase (note.ntitle[j]);
	}
	for (j = 0; j < TITLEN + 1 - xlength; j++) {
	    for (i = 0; i < xlength; i++) {
		if (io->xstring[i] != note.ntitle[j + i]) {
		    i = -1;
		    break;
		}
	    }
	    if (i != -1) {
		return(fromnum);
	    }
	}
	fromnum--;				/* move on to the next one */
    }
    return(0);
}
