static char *sccsid = "@(#)note.c	1.2 2/2/83";

#include "parms.h"
#include "structs.h"
/*
 *	Putnote
 *
 *	Take the given text, and make put it into the note file.
 *	following algorithm:
 *	reserve space for text
 *	write the text
 *	make the note header record
 *	lock the index file
 *	write the note header info
 *	unlock the index file
 *	rewrite the correct text header info
 *
 *
 *	Delnote(io, noteno):  marks the status word as deleted.
 *
 *	Original authors:	Rob Kolstad and Ray Essick	Winter 1980
 *	modified;	Ray Essick	December 1981
 */

putnote (io, where, title, status, note, auth, policy, lockit, addid, fromsys, addtime)
struct io_f *io;
struct daddr_f *where;
char   *title;
struct note_f  *note;
struct auth_f  *auth;
/* policy = true if this is the policy note */
/* addid = fales if we already have an id for the note */
/* addtime = false if we already have a time for the note */
char   *fromsys;		/* whom we recieved it from (routing) */
{
    int     count;
    char   *p;
    int     notenum;

    note->n_nresp = 0;			/* no responses yet */
    if (addtime) {			/* dont if compressing... */
	gettime(&note->n_rcvd);
	gettime(&note->n_lmod);		/* date of last mod is same */
    }
    copyauth(auth, &note->n_auth);		/* move author over */
    strmove(fromsys, note->n_from);		/* who gave it to us */
    note->n_rindx = -1;			/* no place for responses yet */
    note->n_stat = status;		/* director message, deleted, etc */
    note->n_addr.addr = where->addr;	/* where on disk */
    p = note->ntitle;
    count = TITLEN;
    while (count--) {
	*p++ = *title++;			/* move title over */
    }

    if (lockit) {
	lock(io, 'n');			/* CRITICAL */
    }
    getdscr(io, &io->descr);			/* grab notesfile header */
    if (io->descr.d_stat & NFINVALID) {
	if (lockit) {
	    unlock(io, 'n');			/* CRITICAL */
	}
	closenf(io);
	opennf(io, io->nf);				/* get new links */
	printf("Sorry, your note has been lost in a compression");
	fflush(stdout);
	sleep (2);
	return(-1);
    }
    if (addid) {
	strmove(io->descr.d_id.sys, note->n_id.sys);
							/* copy over sys name */
	note->n_id.uniqid = ++(io->descr.d_id.uniqid);
							/* and unique id num */
#ifdef	UNIQPLEX
	note->n_id.uniqid += UNIQPLEX * io->descr.d_nfnum;
							/* mpx in nf num */
#endif
    }

    if (policy) {
	io->descr.d_plcy = 1;		/* mark as having a policy note */
	notenum = 0;
    } else {
	notenum = ++io->descr.d_nnote;		/* this note's number */
    }
    if (addtime) {				/* see if want timestamp */
	gettime(&io->descr.d_lastm);		/* last time file modified */
    }
    putnrec(io, notenum, note);			/* write note info */
    putdscr(io, &io->descr);			/* rewrite header info */
    if (lockit) {
	unlock(io, 'n');			/* CRITICAL */
    }
    io->nnotwrit++;				/* bump count of writes */
    return(io->descr.d_nnote);			/* tell which slot it is in */
}


delnote (io, noteno, lockit)
struct io_f *io;
{
    struct note_f   note;

    if (lockit) {
	lock(io, 'n');				/* CRITICAL */
    }
    getnrec(io, noteno, &note);			/* get the note */
    note.n_stat |= DELETED;			/* deleted */
    putnrec(io, noteno, &note);
    if (lockit) {
	unlock(io, 'n');			/* CRITICAL */
    }
}
