static char *sccsid = "@(#)resp.c	1.2 2/2/83";

#include "parms.h"
#include "structs.h"
#ifdef BSD4.1c
#include <sys/file.h>
#else
#define L_SET 0
#define L_INCR 1
#define L_XTND 2
#endif BSD4.1c

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

long lseek ();				/* declare for type checking */

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
struct when_f  *rcvdtime;		/* time to mark as written */
{
    int     i,
            phys,			/* physical subscript number */
            lastin;			/* address of resp record in memory */
    struct resp_f   resp;
    if (lockit) {
	lock(io, 'n');			/* entirely critical */
    }
    getdscr(io, &io->descr);
    if (io->descr.d_stat & NFINVALID) {
	closenf (io);
	opennf (io, io->nf);
	getdscr (io, &io->descr);	/* and updated descriptor */
	if (lockit) {
	    unlock(io, 'n');			/* entirely critical */
	}
	return(0);
    }
    getnrec(io, noteno, note);
    if ((note->n_stat & DELETED) != 0) {	/* is this note deleted? */
    /* see, it could be deleted by someone else in the intermediary */
	if (lockit) {
	    unlock(io, 'n');			/* entirely critical */
	}
	return(0);				/* putresp failed */
    }
    if (note->n_rindx < 0) {	/* is there an attached response record ? */
	lastin = note->n_rindx = getfrsp(io);	/* no, make one */
	for (i = 0; i < RESPSZ; i++) {
	    resp.r_stat[i] = 0;		/* mark all as undeleted at start */
	}
    } else {
	getrrec(io, lastin = note->n_rindx, &resp); /* get 1st resp record */
    }
    i = phys = 0;			/* logical/phys records start here */
    while (i < note->n_nresp) {		/* until we get to end */
	if (phys >= RESPSZ) {			/* off end? -- need next recd */
	    phys = 0;				/* beginning of next one */
	    getrrec(io, lastin = resp.r_next, &resp);  /* next recd */
	}
	if ((resp.r_stat[phys] & DELETED) == 0) {
	    i++;			/* count this entry if undeleted */
	}
	phys++;				/* always count these */
    }
    /* could have gone off end with last phys++ */
    if (phys >= RESPSZ) {
	phys = 0;
	resp.r_next = getfrsp(io);
	putrrec (io, lastin, &resp);		/* out w/modified link */
	lastin = resp.r_next;
	resp.r_next = -1;			/* helps debugging */
	for (i = 0; i < RESPSZ; i++) {
	    resp.r_stat[i] = 0;			/* mark all as undeleted */
	}
    }
    note->n_nresp++;				/* one more response! */
    resp.r_addr[phys].addr = where->addr;
    if (addtime) {
	gettime(&resp.r_rcvd[phys]);
    } else {
	copydate(rcvdtime, &resp.r_rcvd[phys]);	/* use supplied */
    }
    copydate(adate, &resp.r_when[phys]);	/* copy date over */
    copyauth(auth, &resp.r_auth[phys]);	/* and author */
    strmove(fromsys, resp.r_from[phys]);	/* who gave it to us */
    if (addid) {				/* generate unique id */
	strmove(io->descr.d_id.sys, resp.r_id[phys].sys);
	resp.r_id[phys].uniqid = ++(io->descr.d_id.uniqid);
#ifdef	UNIQPLEX
	resp.r_id[phys].uniqid += UNIQPLEX * io->descr.d_nfnum;
						/* mpx in the nf number */
#endif
    } else {					/* use the supplied unique id */
	strmove(theid->sys, resp.r_id[phys].sys);
	resp.r_id[phys].uniqid = theid->uniqid;
    }
    resp.r_stat[phys] = status;
    if (addtime) {				/* timestamp ? */
	gettime(&note->n_lmod);			/* last modified entire note */
	gettime(&io->descr.d_lastm);		/* last modified entire file */
    }
    putrrec(io, lastin, &resp);
    putnrec(io, noteno, note);
    putdscr(io, &io->descr);	/* order of these three keeps disk consistent */
    if (lockit) {
	unlock(io, 'n');			/* entirely critical */
    }
    io->nrspwrit++;				/* add count of writes */
    return(note->n_nresp);			/* success */
}


getfrsp (io)
struct io_f   *io;
{
    int     i;				/* will contain the free pointer */
    x (lseek(io->fidrdx, 0L, L_SET) < 0, "getfrsp: seek I");
    x (read(io->fidrdx, &i, sizeof(i)) < sizeof(i), "getfrsp: read");
    i++;				/* next free */
    x (lseek(io->fidrdx, 0L, L_SET) < 0, "getfrsp: seek II");
    x (write(io->fidrdx, &i, sizeof(i)) < sizeof(i), "getfrsp: write");
    return(i-1);
}


delresp (io, noteno, resprec, resphys, lockit)
struct io_f *io;
int noteno;
int resprec;
int resphys;
int lockit;
{
    struct resp_f   resp;
    struct note_f   note;
    if (lockit) {
	lock(io, 'n');			/* entirely critical */
    }
    getrrec(io, resprec, &resp);
    if ((resp.r_stat[resphys] & DELETED) == 0) {
			/* makes sure that someone hasn't zapped at same time */
	resp.r_stat[resphys] |= DELETED;		/* deleted */
	putrrec(io, resprec, &resp);
	getnrec(io, noteno, &note);
	--note.n_nresp;
	putnrec(io, noteno, &note);
    }
    if (lockit) {
	unlock(io, 'n');			/* entirely critical */
    }
    return;
}
