#include "parms.h"
#include "structs.h"

#ifdef	RCSIDENT
static char rcsid[] = "$Header: tsearch.c,v 1.7 85/01/18 15:39:59 notes Rel $";
#endif	RCSIDENT

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

    if (grabstring || io -> xstring[0] == '\0')
    {
	at (-1, 1);
	printf ("Search String: ");
	i = gline (io -> xstring, TITLEN);		/* grab one */
	at (-1, 1);
	printf ("%*s", i + 16, " ");			/* clear the line */
	if (i == 1)
	{
	    io -> xstring[0] = '\0';
	    return (-1);
	}
    }
    at (0, 1);
    fromnum = findtitle (io, fromnum, FALSE);
    if (intflag)
    {
	intflag = 0;					/* don't recatch this one */
	printf ("Search aborted");
    }
    if (fromnum > 0)
	return (fromnum);
    else
	printf ("%s: Not Found", io -> xstring);
    return 0;
}

findtitle (io, fromnum, anchored)
struct io_f *io;
int     fromnum;
int     anchored;					/* true if anchored search */
{
    struct note_f   note;				/* hold note descr */
    register int    i,
                    j,
                    xlength;
    register int    nlength;				/* how far into title */

    for (j = 0; io -> xstring[j]; j++)			/* force lower case */
	io -> xstring[j] = tolcase (io -> xstring[j]);
    if (io -> xstring[0] == '\0')			/* if empty then */
	return (-1);					/* don't search */
    xlength = strlen (io -> xstring);
    if (xlength >= TITLEN)
	xlength = TITLEN - 1;				/* only so far */
    if (fromnum > io -> descr.d_nnote)
	fromnum = io -> descr.d_nnote;

    intflag = 0;					/* catch interupts */
    while (fromnum > 0)
    {
	getnrec (io, fromnum, &note);			/* grab descriptor */
	if (note.n_stat & DELETED)
	{
	    fromnum--;
	    continue;					/* skip this one */
	}
	for (j = 0; j < TITLEN && note.ntitle[j]; j++)
	    note.ntitle[j] = tolcase (note.ntitle[j]);
	if (anchored)
	{
	    nlength = 1;				/* must start at LHS */
	}
	else
	{
	    nlength = TITLEN + 1 - xlength;		/* let it float */
	}
	for (j = 0; j < nlength; j++)
	{
	    for (i = 0; i < xlength && (j + i) < TITLEN; i++)
	    {
#ifdef	notdef
		if (note.ntitle[j + i] == '\0')
		{
		    /* 
		     * something for strings running off the end
		     * mostly for USENET title truncation
		     */
		}
#endif	notdef
		if (io -> xstring[i] != note.ntitle[j + i])
		{					/* true at eostring */
		    i = (-1);				/* not here */
		    break;
		}
	    }
	    if (i != -1)				/* matched */
		return fromnum;
	}
	fromnum--;					/* try next note */
	if (intflag)
	    break;					/* interupt */
    }
    return (0);
}
