#include "parms.h"
#include "structs.h" 

#ifdef	RCSIDENT
static char rcsid[] = "$Header: asearch.c,v 1.7 85/01/18 15:04:25 notes Rel $";
#endif	RCSIDENT

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

asearch (io, notenum, respnum, grabname)
struct io_f *io;
int    *notenum,
       *respnum;
{
    struct note_f   note;
    struct resp_f   rsprec;
    register int    i,
                    j;					/* scratch counters */
    int     rblock,
            roffset;
    char    buf[SYSSZ + NAMESZ + 5];
    char    author[SYSSZ + NAMESZ + 2];			/* must hold either */
    char    checkit[SYSSZ + NAMESZ + 2];		/* built from note */

    if (grabname || (io -> xauthor[0] == '\0'))
    {							/* get an author */
rekey: 							/* re-enter */
	at (-1, 1);
	printf ("Search author:                          ");
	at (-1, 16);
	i = gline (buf, NNLEN + SYSSZ + 1);		/* grab name */
	at (-1, 1);
	printf ("%*s", i + 15, " ");
	if (i == 1)
	    return (-1);				/* no name */
	if (sscanf (buf, "%s", author) == 1)
	{
	    strcpy (io -> xauthor, author);		/* load it */
	}
	else
	{
	    at (0, 1);
	    printf ("Bad author specification - reenter");
	    goto rekey;
	}
    }

    at (-1, 1);
    printf ("Searching for articles by %s    ", io -> xauthor);
    fflush (stdout);

    if (*notenum > io -> descr.d_nnote)			/* check boundaries */
    {
	*respnum = 0;
	*notenum = io -> descr.d_nnote;
    }

    for (j = 0; io -> xauthor[j]; j++)			/* force lower case */
	io -> xauthor[j] = tolcase (io -> xauthor[j]);

    if (*respnum != 0)
    {
	getnrec (io, *notenum, &note);
	goto inloop;
    }

    intflag = 0;					/* for quit signals */
    while (*notenum > 0)
    {
	getnrec (io, *notenum, &note);
	if (note.n_stat & DELETED)
	{
	    (*notenum)--;
	    continue;					/* dead note */
	}
#ifdef	USERHOST
	sprintf (checkit, "%s@%s", note.n_auth.aname, note.n_id.sys);
#else
	sprintf (checkit, "%s!%s", note.n_id.sys, note.n_auth.aname);
#endif	USERHOST
	for (j = strlen (checkit) - 1; j >= 0; j--)	/* use lower case */
	    checkit[j] = tolcase (checkit[j]);
	if (substr (io -> xauthor, checkit))
	    return (*notenum);				/* found him! */
	*respnum = 1;					/* set it now */
inloop: 						/* start on a resp */
	for (; *respnum <= note.n_nresp; (*respnum)++)
	{
	    if (lrsp (io, *notenum, *respnum, &rsprec, &roffset, &rblock) == -1)
		break;
#ifdef	USERHOST
	    sprintf (checkit, "%s@%s", rsprec.r_auth[roffset].aname,
		    rsprec.r_id[roffset].sys);
#else
	    sprintf (checkit, "%s!%s", rsprec.r_id[roffset].sys,
		    rsprec.r_auth[roffset].aname);
#endif	USERHOST
	    if (substr (io -> xauthor, checkit))
		return * notenum;			/* found him */
	    if (intflag)
		break;					/* impatience */
	}
	*respnum = 0;					/* make it a main note */
	(*notenum)--;					/* and proceed to next note */
	if (intflag)
	    break;					/* impatient little boy */
    }
    at (0, 1);
    if (intflag)
    {
	intflag = 0;					/* don't field same one later */
	printf ("Search aborted");
    }
    else
    {
	printf ("Can't find any articles by %s", io -> xauthor);
    }
    return 0;
}
