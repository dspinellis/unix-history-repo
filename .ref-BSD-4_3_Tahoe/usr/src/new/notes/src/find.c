#include "parms.h"
#include "structs.h"

#ifdef	RCSIDENT
static char rcsid[] = "$Header: find.c,v 1.7.0.3 85/04/05 15:22:05 notes Rel $";
#endif	RCSIDENT

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
    register int    i;
#ifdef	IDCOMPAT
    register int    wantlen;				/* length of wanted */
    register char  *wantdomain;				/* has "." */
    register int    thislen;				/* current id */
    register char  *thisdomain;				/* current id has . */
    register int    shortlen;				/* shorter */
#endif	IDCOMPAT

#ifdef	IDCOMPAT
    wantlen = strlen (noteid -> sys);
    wantdomain = index (noteid -> sys, '.');
#endif	IDCOMPAT

    for (i = io -> descr.d_nnote; i > 0; i--)		/* dups likely "new" */
    {							/* so search backwards */
	getnrec (io, i, note);
	if (note -> n_stat & DELETED)
	    continue;					/* ignore deleted */
	if (noteid -> uniqid == note -> n_id.uniqid)	/* cheap to do */
	{
	    if (strcmp (noteid -> sys, note -> n_id.sys) == 0)/* more costly */
	    {
		return (i);
	    }
#ifdef	IDCOMPAT
	    /* 
	     * check for truncated or otherwise mangled ids 
	     * THIS CODE IS NOT WELL TESTED AND PROBABLY DOESN'T WORK
	     * QUITE RIGHT.
	     */
	    thislen = strlen (note -> n_id.sys);
	    thisdomain = index (note -> n_id.sys, '.');
	    shortlen = thislen < wantlen ? thislen : wantlen;/* get short */
	    if (thisdomain == (char *) NULL || wantdomain == (char *) NULL)
	    {						/* undomained */
		if (strncmp (note -> n_id.sys, noteid -> sys, shortlen) == 0)
		    return (i);				/* found */
	    }
	    if (shortlen == OLDSYSSZ - 1 || shortlen == OLDSYSSZ)
	    {						/* old format chop */
		if (strncmp (note -> n_id.sys, noteid -> sys, shortlen) == 0)
		    return (i);				/* found */
	    }
	    if (strncmp (note -> n_id.sys, noteid -> sys, OLDSYSSZ - 1) == 0)
	    {						/* match in 1st 10 */
		return (i);
	    }
#endif	IDCOMPAT
	}
    }
    return (0);						/* not found */
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
    register int    i;
    register int    prec;
    register int    poffset;
#ifdef	IDCOMPAT
    register int    wantlen;				/* length of wanted */
    register char  *wantdomain;				/* has "." */
    register int    thislen;				/* current id */
    register char  *thisdomain;				/* current id has . */
    register int    shortlen;				/* shorter */
#endif	IDCOMPAT

    if (note -> n_nresp <= 0)				/* no responses */
	return (0);					/* so it can't be there */
#ifdef	IDCOMPAT
    wantlen = strlen (respid -> sys);			/* get constants */
    wantdomain = index (respid -> sys, '.');
#endif	IDCOMPAT
    prec = note -> n_rindx;				/* get first block */
    poffset = 0;
    getrrec (io, prec, &rrec);
    for (i = 1; i <= note -> n_nresp; i++)		/* through responses */
    {
	while (rrec.r_stat[poffset] & DELETED)
	{						/* skip deleted ones */
	    if (++poffset == RESPSZ)
	    {
		poffset = 0;
		if ((prec = rrec.r_next) == -1)
		    return (0);				/* broken chain */
		getrrec (io, prec, &rrec);		/* passed this buffer */
	    }
	}
	if (respid -> uniqid == rrec.r_id[poffset].uniqid)
	{
	    if (strcmp (respid -> sys, rrec.r_id[poffset].sys) == 0)
	    {
		return (i);				/* return resp number */
	    }
#ifdef	IDCOMPAT
	    /* 
	     * check for truncated and otherwise mangled id's
	     */
	    thislen = strlen (rrec.r_id[poffset].sys);
	    thisdomain = index (rrec.r_id[poffset].sys, '.');
	    shortlen = thislen < wantlen ? thislen : wantlen;/* shorter */
	    if (thisdomain == (char *) NULL || wantdomain == (char *) NULL)
	    {						/* undomained */
		if (strncmp (respid -> sys, rrec.r_id[poffset].sys, shortlen) == 0)
		    return (i);
	    }
	    if (shortlen == OLDSYSSZ - 1 || shortlen == OLDSYSSZ)
	    {						/* old format chop */
		if (strncmp (respid -> sys, rrec.r_id[poffset].sys, shortlen) == 0)
		    return (i);
	    }
	    if (strncmp (respid -> sys, rrec.r_id[poffset].sys, OLDSYSSZ - 1) == 0)
	    {						/* match first 10 */
		return (i);
	    }
#endif	IDCOMPAT
	}
	rrec.r_stat[poffset] |= DELETED;		/* force scan above */
    }
    return (0);						/* is not a response to this note */
}
