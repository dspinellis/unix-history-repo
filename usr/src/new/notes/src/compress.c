#include "parms.h"
#include "structs.h"

#ifdef	RCSIDENT
static char rcsid[] = "$Header: compress.c,v 1.7.0.2 85/03/19 13:02:07 notes Rel $";
#endif	RCSIDENT

/*
 *	compress(io) struct io_f
 *	compresses the notefile specified. All wasted space
 *	reclaimed. The process is a simple one which, like dcheck,
 *	does not work so well on active file systems.
 *	As a consequence, the director options (which call this)
 *	require the notefile to be closed before allowing compression
 *	to take place. 
 *	The code generates 3 scratch files, corresponding with the
 *	two index files and the text file. These are made to
 *	represent a virgin notefile. The descriptor is copied over
 *	with the appropriate fields zapped, and then we go through
 *	a cycle of (read note; write note; (read resp; write resp))
 *	until all the notes and responses are moved over.
 *	the new files are then copied back into place.
 *	
 *	Returns:	0 - all successful
 *			otherwise will core dump with the notefile
 *			in a shambles from the users view point,
 *			but still recoverable by a hotshot-pro.
 *
 *	Original Coding:	Ray Essick	January 1981
 */

compress (io, lockflag, verbosity, numnotes, numresps)
struct io_f *io;
int     lockflag;					/* to lock or not */
int     verbosity;
/* verbosity 	== 0	silent
 *		else	print dots as go & do totals
 */
int    *numnotes,					/* remaining notes when done */
       *numresps;					/* ditto for responses */
{
    struct io_f tmpio;					/* scratch notefile */
    struct note_f   note;				/* hold a note record */
    struct resp_f   resp;				/* hold the response format */
    char    fn1[WDLEN],
            fn2[WDLEN],
            fn3[WDLEN],
            on1[WDLEN],
            on2[WDLEN],
            on3[WDLEN],
            txtfn[WDLEN];				/* hold text going between files */
    struct daddr_f  where;
    FILE * txtfile;
    int     nnotes,
            nresps,
            dint,
            roffset,
            num,
            rblock;
    register int    newnum,
                    presps,
                    rnum;
    struct daddr_f  daddr;
    int     old_umask;					/* save it */


/*
 *	build names of files - in notefile directory
 */

    /* new files */
    sprintf (fn1, "%s/%s/%s%s", io -> basedir, io -> nf, COMPRESS, INDEXN);
    sprintf (fn2, "%s/%s/%s%s", io -> basedir, io -> nf, COMPRESS, INDEXR);
    sprintf (fn3, "%s/%s/%s%s", io -> basedir, io -> nf, COMPRESS, TEXT);

    sprintf (on1, "%s/%s/%s", io -> basedir, io -> nf, INDEXN);/* old files */
    sprintf (on2, "%s/%s/%s", io -> basedir, io -> nf, INDEXR);
    sprintf (on3, "%s/%s/%s", io -> basedir, io -> nf, TEXT);

    old_umask = umask (0);				/* wide open */
    x ((tmpio.fidndx = creat (fn1, 0660)) < 0, "compress: create nindex");
    x ((tmpio.fidrdx = creat (fn2, 0660)) < 0, "compress: create rindex");
    x ((tmpio.fidtxt = creat (fn3, 0660)) < 0, "compress: create txt");

    dint = 0;						/* resp index free pointer */
    daddr.addr = sizeof daddr;				/* and for text file */
    x (write (tmpio.fidrdx, &dint, sizeof dint) != sizeof dint, "compress: resp ptr");
    x (write (tmpio.fidtxt, &daddr, sizeof daddr) != sizeof daddr, "Compress: text ptr");


    closenf (&tmpio);					/* close them up */

    x ((tmpio.fidndx = open (fn1, 2)) < 0, "compress: reopen 1");
							/* open R/W */
    x ((tmpio.fidrdx = open (fn2, 2)) < 0, "compress: reopen 2");
    x ((tmpio.fidtxt = open (fn3, 2)) < 0, "compress: reopen 3");

    strcpy (tmpio.nf, io -> nf);			/* notesfile name */
    strcpy (tmpio.basedir, io -> basedir);		/* and directory */
    nnotes = nresps = 0;
    sprintf (txtfn, "/tmp/nf%d", getpid ());		/* scratch for text */

    if (lockflag)
	locknf (io, DSCRLOCK);				/* lock up the notefile */
    getdscr (io, &tmpio.descr);				/* grab descriptor */
    if (io -> descr.d_stat & NFINVALID)
    {
	printf ("Notesfile compressed behind your back");
	if (lockflag)
	    unlocknf (io, DSCRLOCK);
	closenf (&tmpio);				/* clean up mess */
	x (unlink (fn1) < 0, "compress: unlink tmp1");
	x (unlink (fn2) < 0, "compress: unlink tmp2");
	x (unlink (fn3) < 0, "compress: unlink tmp3");
	umask (old_umask);				/* restore */
	return (-1);
    }

    locknf (io, TXTLOCK);				/* always */

    tmpio.descr.d_nnote = 0;				/* reset note count */
    tmpio.descr.d_delnote = 0;				/* no holes */
    tmpio.descr.d_delresp = 0;
    putdscr (&tmpio, &tmpio.descr);			/* place it into the file */

    if (io -> descr.d_plcy)				/* copy the policy note over */
    {
	getnrec (io, 0, &note);				/* descriptor */
#ifdef	notdef
	x ((txtfile = fopen (txtfn, "w")) == NULL, "compress:bad txt");
	pageout (io, &note.n_addr, txtfile);
	fclose (txtfile);
	x ((txtfile = fopen (txtfn, "r")) == NULL, "compress: bad txt read");
	pagein (&tmpio, txtfile, &where);
	fclose (txtfile);
#else
	pagemove (io, &note.n_addr, &tmpio, &where, NOLOCKIT);
#endif
#ifdef	FIXTIMES
	fixtime (&note.n_rcvd);
	fixtime (&note.n_lmod);
	fixtime (&note.n_date);
#endif	FIXTIME
	putnote (&tmpio, &where, note.ntitle, note.n_stat, &note, &note.n_auth,
		POLICY, NOLOCKIT, NOADDID, note.n_from, NOADDTIME);
    }
    for (num = 1; num <= io -> descr.d_nnote; num++)
    {
	if (verbosity)					/* if being noisy */
	{
	    putchar ('.');
	    fflush (stdout);				/* so he see action */
	}
	getnrec (io, num, &note);
	if (note.n_stat & DELETED)
	    continue;					/* deleted - we throw away */
#ifdef	notdef
	x ((txtfile = fopen (txtfn, "w")) == NULL, "compress:bad txt");
	pageout (io, &note.n_addr, txtfile);
	fclose (txtfile);
	x ((txtfile = fopen (txtfn, "r")) == NULL, "compress: bad txt read");
	pagein (&tmpio, txtfile, &where);
	fclose (txtfile);
#else
	pagemove (io, &note.n_addr, &tmpio, &where, NOLOCKIT);
#endif
	presps = note.n_nresp;				/* save max number of responses */
#ifdef	FIXTIMES
	fixtime (&note.n_rcvd);
	fixtime (&note.n_lmod);
	fixtime (&note.n_date);
#endif	FIXTIME
	newnum = putnote (&tmpio, &where, note.ntitle, note.n_stat, &note, &note.n_auth,
		NOPOLICY, NOLOCKIT, NOADDID, note.n_from, NOADDTIME);
	nnotes++;					/* add a note */

	for (rnum = 1; rnum <= presps; rnum++)		/* process responses */
	{
	    if (lrsp (io, num, rnum, &resp, &roffset, &rblock) != 0)
		break;					/* bad response chain - drop rest */
#ifdef	notdef
	    x ((txtfile = fopen (txtfn, "w")) == NULL, "compress:bad txt");
	    pageout (io, &resp.r_addr[roffset], txtfile);
	    fclose (txtfile);
	    x ((txtfile = fopen (txtfn, "r")) == NULL, "compress: bad txt read");
	    pagein (&tmpio, txtfile, &where);
	    fclose (txtfile);
#else
	    pagemove (io, &resp.r_addr[roffset], &tmpio, &where, NOLOCKIT);
#endif
#ifdef	FIXTIMES
	    fixtime (&resp.r_when[roffset]);
	    fixtime (&resp.r_rcvd[roffset]);
#endif	FIXTIMES
	    putresp (&tmpio, &where, resp.r_stat[roffset], newnum, &resp.r_when[roffset],
		    &resp.r_auth[roffset], &note, NOLOCKIT, &resp.r_id[roffset],
		    NOADDID, resp.r_from[roffset], NOADDTIME, &resp.r_rcvd[roffset]);
	    nresps++;					/* count responses */
	}
    }

/*	well, we have now copied the entire notefile over, so the time
 *	has come to move it back into the correct file names - we will
 *	do this by 
 */
    closenf (&tmpio);					/* close the new one */

    getdscr (io, &io -> descr);
    io -> descr.d_stat |= NFINVALID;			/* mark it bad */
    putdscr (io, &io -> descr);
    closenf (io);					/* close the old one */

    x (unlink (on1) < 0, "compress: remove old 1");
    x (link (fn1, on1) < 0, "compress: link new 1");
    x (unlink (fn1) < 0, "compress: remove tmp 1");
    x (unlink (on2) < 0, "compress: remove old 2");
    x (link (fn2, on2) < 0, "compress: link new 2");
    x (unlink (fn2) < 0, "compress: remove tmp 2");
    x (unlink (on3) < 0, "compress: remove old 3");
    x (link (fn3, on3) < 0, "compress: link new 3");
    x (unlink (fn3) < 0, "compress: remove tmp 3");

    opennf (io, (char *) NULL);				/* relink to new one */

    getdscr (io, &io -> descr);				/* get new descr */

    if (lockflag)
	unlocknf (io, DSCRLOCK);			/* release the locks */
    unlocknf (io, TXTLOCK);				/* always text lock */
#ifdef	notdef
    unlink (txtfn);
#endif
    *numnotes = nnotes;					/* fill in callers values */
    *numresps = nresps;
    umask (old_umask);					/* restore */
    return 0;						/* return ok */
}

#ifdef	FIXTIMES
static  fixtime (when)
struct when_f  *when;
{
    struct when_f   built;

    if (when -> w_gmttime == 0)
	return;						/* already ok */
    if (when -> w_gmttime < 0)
    {
	when -> w_gmttime = 0;
	return;
    }
    maketime (&built, when -> w_gmttime);
    if (built.w_year != when -> w_year ||
	    built.w_month != when -> w_month ||
	    built.w_day != when -> w_day ||
	    built.w_hours != when -> w_hours ||
	    built.w_mins != when -> w_mins)
	when -> w_gmttime = 0;				/* zero it */
}
#endif	FIXTIMES
