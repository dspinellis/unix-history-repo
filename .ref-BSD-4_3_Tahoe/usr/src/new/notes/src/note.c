#ifdef	RCSIDENT
static char rcsid[] = "$Header: note.c,v 1.7 85/01/18 15:32:57 notes Rel $";
#endif	RCSIDENT

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

#include "parms.h"
#include "structs.h"

putnote (io, where, title, status, note, auth, policy, lockit, addid, fromsys, addtime)
struct io_f *io;
struct daddr_f *where;
char   *title;
struct note_f  *note;
struct auth_f  *auth;
/* policy = true if this is the policy note */
/* addid = fales if we already have an id for the note */
/* addtime = false if we already have a time for the note */
char   *fromsys;					/* whom we recieved it from (routing) */
{
    int     count;
    char   *p;
    int     notenum;

    note -> n_nresp = 0;				/* no responses yet */
    if (addtime)					/* dont if compressing... */
    {
	gettime (&note -> n_rcvd);
	gettime (&note -> n_lmod);			/* date of last mod is same */
    }
    note -> n_auth = *auth;				/* move author over */
    strcpy (note -> n_from, fromsys);			/* who gave it to us */
    note -> n_rindx = (-1);				/* no place for responses yet */
    note -> n_stat = status;				/* director message, deleted, etc */
    note -> n_addr = *where;				/* where on disk */
    strncpy (note -> ntitle, title, TITLEN);		/* copy */
    note -> ntitle[TITLEN - 1] = '\0';			/* stop for sure */

    if (lockit)
	locknf (io, DSCRLOCK);				/* BEGIN CRITICAL SECTION */
    getdscr (io, &io -> descr);				/* grab notesfile header */
    if (io -> descr.d_stat & NFINVALID)
    {
	if (lockit)
	    unlocknf (io, DSCRLOCK);
	closenf (io);
	opennf (io, 0);					/* get new links */
	printf ("Sorry, your note has been lost in a compression");
/*
 *	Should give the user a chance to recover his text somewhere
 *	in here 
 */
	fflush (stdout);
	sleep (1);
	return (-1);
    }
    if (addid)
    {
#ifdef	SHAREDATA
	strmove (System, note -> n_id.sys);		/* copy sys name */
#else	! SHAREDATA
	strmove (io -> descr.d_id.sys, note -> n_id.sys);
#endif	SHAREDATA
	note -> n_id.uniqid = ++(io -> descr.d_id.uniqid);
							/* and unique id num */
#ifdef	UNIQPLEX
	note -> n_id.uniqid += UNIQPLEX * io -> descr.d_nfnum;
							/* mpx in nf num */
#endif
    }

    if (policy)
    {
	io -> descr.d_plcy = 1;				/* mark as having a policy note */
	notenum = 0;
    }
    else
	notenum = ++io -> descr.d_nnote;		/* this note's number */
    if (addtime)					/* see if want timestamp */
	gettime (&io -> descr.d_lastm);			/* last time file modified */
    putnrec (io, notenum, note);			/* write note info */
    putdscr (io, &io -> descr);				/* rewrite header info */
    if (lockit)
	unlocknf (io, DSCRLOCK);			/* END CRITICAL SECTION */
    io -> nnotwrit++;					/* bump count of writes */
    return (io -> descr.d_nnote);			/* tell which slot it is in */
}


delnote (io, noteno, lockit)
struct io_f *io;
{
    struct note_f   note;

    if (lockit)
	locknf (io, DSCRLOCK);				/* CRITICAL */
    getnrec (io, noteno, &note);			/* get the note */
    note.n_stat |= DELETED;				/* deleted */
    putnrec (io, noteno, &note);
    getdscr (io, &io -> descr);				/* update delete count */
    io -> descr.d_delnote++;
    io -> descr.d_delresp += note.n_nresp;		/* count resps */
    putdscr (io, &io -> descr);
    if (lockit)
	unlocknf (io, DSCRLOCK);			/* END CRITICAL */
}
