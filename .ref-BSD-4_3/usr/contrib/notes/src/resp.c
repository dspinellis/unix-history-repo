#include	"parms.h"
#include	"structs.h"

#ifdef	RCSIDENT
static char rcsid[] = "$Header: resp.c,v 1.7 85/01/18 15:38:30 notes Rel $";
#endif	RCSIDENT

/*
 * putresp (io, text, status, noteno, anon)
 *
 *	writes out a response to noteno in the last position.
 *	returns 0 to indicate note has been deleted,
 *	otherwise it returns the response number it inserted.
 *
 *
 * delresp(io, noteno, resprec, resphys)
 *
 *	Deletes PHYSICAL response located at resprec (record id)
 *	resphys (internal subscript), updates note's response count
 *
 *
 * getfrsp(io) gets the next free response index -- simple free list chained
 *   off first two bytes of file, currently.
 *
 */

long    lseek ();					/* declare for type checking */

putresp (io, where, status, noteno, adate, auth, note, lockit, theid, addid, fromsys,
    addtime, rcvdtime)
							/* all input params */
struct io_f *io;
struct daddr_f *where;
struct note_f  *note;
struct when_f  *adate;
struct auth_f  *auth;
struct id_f *theid;
char   *fromsys;
/* addtime - whether to modify time stamps - useed for compression */
struct when_f  *rcvdtime;				/* time to mark as written */
{
    int     i,
            phys,					/* physical subscript number */
            lastin;					/* address of resp record in memory */
    struct resp_f   resp;

    if (lockit)
	locknf (io, DSCRLOCK);				/* entirely critical */
    getdscr (io, &io -> descr);
    if (io -> descr.d_stat & NFINVALID)
    {
	closenf (io);
	opennf (io, 0);
	getdscr (io, &io -> descr);			/* and updated descriptor */
	if (lockit)
	    unlocknf (io, DSCRLOCK);
	return 0;
    }
    getnrec (io, noteno, note);
    if ((note -> n_stat & DELETED) != 0)		/* is note gone? */
    {
	/* 
	 * see, it could be deleted by someone else in the
	 * mean time...
	 */
	if (lockit)
	    unlocknf (io, DSCRLOCK);			/* not so critical now */
	return 0;					/* putresp failed */
    }
    if (note -> n_rindx < 0)				/* is there an attached response record ? */
    {
	lastin = note -> n_rindx = getfrsp (io);	/* no, make one */
	resp.r_first = 1;
	resp.r_last = 0;				/* counts */
	resp.r_previous = (-1);				/* no previous */
	resp.r_next = (-1);				/* no next */
	for (i = 0; i < RESPSZ; i++)
	    resp.r_stat[i] = 0;				/* mark all as undeleted at start */
    }
    else
	getrrec (io, lastin = note -> n_rindx, &resp);	/* get first resp record */
    i = phys = 0;					/* logical/phys records start here */
/*
 *	should update this to take advantage of r_first and r_last
 *	as it would speed up writing responses in long note strings.
 */
    while (i < note -> n_nresp)				/* until we get to end */
    {
	if (phys >= RESPSZ)				/* off end? -- need next recd */
	{
	    phys = 0;					/* beginning of next one */
	    getrrec (io, lastin = resp.r_next, &resp);	/* next recd */
	}
	if ((resp.r_stat[phys] & DELETED) == 0)
	    i++;					/* count this entry if undeleted */
	phys++;						/* always count these */
    }
    /* 
     * could have gone off end with last phys++
     */
    if (phys >= RESPSZ)
    {
	phys = 0;
	resp.r_next = getfrsp (io);
	putrrec (io, lastin, &resp);			/* out w/modified link */
	resp.r_previous = lastin;			/* back ptr */
	lastin = resp.r_next;
	resp.r_next = (-1);				/* helps debugging */
	resp.r_first = note -> n_nresp + 1;		/* front and */
	resp.r_last = resp.r_first - 1;			/* last. */
	/* 
	 * r_last is bumped just before the putrrec() below.
	 */
	for (i = 0; i < RESPSZ; i++)
	{
	    resp.r_stat[i] = 0;				/* mark all as undeleted */
	}
    }
    note -> n_nresp++;					/* one more response! */
    resp.r_addr[phys] = *where;
    if (addtime)
	gettime (&resp.r_rcvd[phys]);
    else
	copydate (rcvdtime, &resp.r_rcvd[phys]);	/* use supplied */
    copydate (adate, &resp.r_when[phys]);		/* copy date over */
    copyauth (auth, &resp.r_auth[phys]);		/* and author */
    strmove (fromsys, resp.r_from[phys]);		/* who gave it to us */
    if (addid)						/* generate unique id */
    {
#ifdef	SHAREDATA
	strmove (System, resp.r_id[phys].sys);		/* load sys name */
#else	! SHAREDATA
	strmove (io -> descr.d_id.sys, resp.r_id[phys].sys);
#endif	SHAREDATA

	resp.r_id[phys].uniqid = ++(io -> descr.d_id.uniqid);
#if	defined(UNIQPLEX)
	resp.r_id[phys].uniqid += UNIQPLEX * io -> descr.d_nfnum;
							/* mpx in the nf number */
#endif	defined(UNIQPLEX)
    }
    else
    {							/* use the supplied unique id */
	strmove (theid -> sys, resp.r_id[phys].sys);
	resp.r_id[phys].uniqid = theid -> uniqid;
    }
    resp.r_stat[phys] = status;
    if (addtime)					/* timestamp ? */
    {
	gettime (&note -> n_lmod);			/* last modified entire note */
	gettime (&io -> descr.d_lastm);			/* last modified entire file */
    }
    resp.r_last++;					/* 1 more there */
    putrrec (io, lastin, &resp);
    putnrec (io, noteno, note);
    putdscr (io, &io -> descr);				/* order of these three keeps disk consistent */
    if (lockit)
	unlocknf (io, DSCRLOCK);
    io -> nrspwrit++;					/* add count of writes */
    return note -> n_nresp;				/* success */
}


/*
 *	getfrsp()
 *
 *	get the next open response block.
 */
getfrsp (io) struct io_f   *io;
{
    int     i;						/* will contain the free pointer */
    x (lseek (io -> fidrdx, 0L, 0) < 0, "getfrsp: seek I");
    x (read (io -> fidrdx, &i, sizeof i) < sizeof i, "getfrsp: read");
    i++;						/* next free */
    x (lseek (io -> fidrdx, 0L, 0) < 0, "getfrsp: seek II");
    x (write (io -> fidrdx, &i, sizeof i) < sizeof i, "getfrsp: write");
    return i - 1;
}


/*
 *	delresp()
 *
 *	delete a PHYSICAL response.  This takes the actual place
 *	the response lives in rather than the logical response
 *	number.
 *
 *	Updates r_first and r_last for the rest of the response
 *	chain.
 */
delresp (io, noteno, resprec, resphys, lockit)
struct io_f *io;
{
    struct resp_f   resp;
    struct note_f   note;
    register int    i;					/* follows resp chain */

    if (lockit)
	locknf (io, DSCRLOCK);				/* all critical */
    getrrec (io, resprec, &resp);
    if ((resp.r_stat[resphys] & DELETED) == 0)
    {
	/* 
	 * makes sure that someone hasn't zapped at same time
	 */
	resp.r_stat[resphys] |= DELETED;		/* deleted */
	resp.r_last--;					/* fix count */
	/* 
	 *	if r_first > r_last, we have an empty block.
	 *	unfortunate waste of space that is rectified
	 *	by compression later.
	 */
	putrrec (io, resprec, &resp);
	while ((i = resp.r_next) >= 0)			/* for rest of chain */
	{
	    getrrec (io, i, &resp);			/* get it */
	    resp.r_first--;
	    resp.r_last--;				/* fix indices */
	    putrrec (io, i, &resp);			/* and replace */
	}
	getnrec (io, noteno, &note);			/* update the note */
	--note.n_nresp;
	putnrec (io, noteno, &note);
	getdscr (io, &io -> descr);			/* count deletes */
	io -> descr.d_delresp++;			/* used by compress */
	putdscr (io, &io -> descr);
    }
    if (lockit)
	unlocknf (io, DSCRLOCK);
    return;
}
