#include "parms.h"
#include "structs.h"
#include "net.h"
#ifdef	FASTSEQ
#include	<sys/types.h>
#include	<sys/stat.h>
#endif	FASTSEQ

#ifdef	RCSIDENT
static char rcsid[] = "$Header: nfsend.c,v 1.7.0.1 85/08/04 12:16:32 notes Rel $";
#endif	RCSIDENT

/*
 *	nfsend will send the notes/responses to a new site that
 *	that site has not seen (at least as far as we are concerned)
 *	This means that we send them the ones that we recieved later
 *	then the last time we sent them, and also that were not
 *	written there nor were routed through there.
 *
 *	Also, a logfile is maintained of the network transmissions made.
 *
 *	Returns:	-1 if the notefile is not a networked notefile
 *			else count of articles sent
 *
 *	Original Coding:	Ray Essick	December 1981
 */

static struct io_f  io;
static struct note_f    note;
static struct resp_f    rsprec;
static int  ncount,
            rcount,					/* how many of each sent */
            num,
            rrecnum,
            roffset,
            respnum;
static  FILE * log,
       *zfile;						/* logfile stuff */
static struct when_f    entered;
static char line[DATELEN];				/* formatted date */
static char fn[WDLEN];					/* network log */
static char scrfile[WDLEN];				/* scratch file */
static char sendtime[DATELEN];				/* current time */
static struct when_f    xsendtime;			/* raw current time */
static int  traffic;					/* if new traffic */
static int  opened;					/* for FASTSEQ */

nfsend (tosite, nfname, dmpfile, usetime, sendhim, proto)
char   *tosite,						/* destination site */
       *nfname,						/* notefile sending */
       *dmpfile;					/* temp file */
{

    traffic = 0;					/* idle */
    opened = 0;						/* FASTSEQ idle */
    strcpy (scrfile, "");				/* mark as unused */
    rcount = ncount = 0;				/* count sent */
    gettime (&xsendtime);
    sprdate (&xsendtime, sendtime);			/* format xmit time */
    sprintf (Seqname, "Sy:%s", tosite);			/* sequencer name */


#ifdef	FASTSEQ
    {
	struct when_f   whenvec;
	char    NoteFile[WDLEN];
	struct stat StatBuf;

	if (nfname[0] == '/')				/* absolute pathname */
	{
	    getlast (&io.stime, rindex (nfname, '/') + 1, usetime, Seqname);
	    sprintf (NoteFile, "%s/%s", nfname, TEXT);
	}
	else						/* relative to Mstdir */
	{
	    getlast (&io.stime, nfname, usetime, Seqname);
	    sprintf (NoteFile, "%s/%s/%s", Mstdir, nfname, TEXT);
	}
	if (stat (NoteFile, &StatBuf) >= 0)
	{
	    maketime (&whenvec, (long) StatBuf.st_mtime);
	    if (inorder (&whenvec, &io.stime))
	    {
		return (0);				/* nothing new */
	    }
	}
    }
#endif	FASTSEQ

    if (init (&io, nfname) < 0)
    {
	printf ("Couldn't open %s, try again later\n", nfname);
	fflush (stdout);
	return (-1);
    }

    opened++;						/* nfsenddone will close */

    getperms (&io, 1, tosite);				/* grab system name */
    if (!allow (&io, READOK))				/* read permission? */
    {
	printf ("Site %s has no read permission for %s\n",
		tosite, nfname);
	fflush (stdout);
	return (-1);					/* and go back */
    }

    if ((io.descr.d_stat & NETWRKD) == 0)
    {
	printf ("Notefile %s is not networked\n", nfname);
	fflush (stdout);
	return (-1);
    }


    gettime (&entered);					/* for seq. update */
    getlast (&io.stime, io.nf, usetime, Seqname);	/* grab the time */

    if (!inorder (&io.stime, &io.descr.d_lastm))	/* something new */
    {							/* idle notesfile */
#ifdef	FASTSEQ
/*
 *	update the sequencer so the next pass through will be 
 *	caught by the FASTSEQ test which just stats the file.
 */
	fixlast (&entered, io.nf, NORMSEQ, Seqname);	/* update sequencer */
#endif	FASTSEQ
	return (0);
    }
/*
 *	reach here only if we have potential traffic
 *	and permission to send it.
 */
    traffic++;						/* forces seq update */

    x ((zfile = fopen (dmpfile, "w")) == NULL, "nfsend: scratch");
    strcpy (scrfile, dmpfile);				/* save scratch name */
    num = 0;						/* start at the beginning */
    while ((num = nxtnote (&io, num, &io.stime)) != -1)
    {
	getnrec (&io, num, &note);			/* grab the header */
	if (!strcmp (note.n_from, tosite) && !(sendhim & SENDHIS))
	    goto doresps;				/* they sent it to us */
	if (!strcmp (note.n_id.sys, tosite) && !(sendhim & SENDHIS))
	    goto doresps;				/* written there */
	if (inorder (&io.stime, &note.n_rcvd) == 0)
	    goto doresps;				/* only modified */
	if ((note.n_stat & FRMNEWS) && !(sendhim & SENDNEWS))/* DO NOT send news */
	    goto doresps;				/* causes uniqid probs */
#ifdef	notdef
	/* 
	 * we want to send foster parents so they will have a title on
	 * the remote end.
	 */
	if (note.n_stat & ORPHND)			/* no foster parents */
	    goto doresps;				/* should have copy */
#endif	notdef
	dmpnote (&io, &note, num, zfile, NODETAIL, proto);/* dump to output */
	ncount++;					/* bump count of sent articles */
	io.nnotxmit++;					/* and global stats */

doresps: 						/* process the responses */
	respnum = 0;
	while ((respnum = nxtresp (&io, num, respnum, &io.stime)) != -1)
	{
	    if (lrsp (&io, num, respnum, &rsprec, &roffset, &rrecnum) == -1)
		break;					/* no response */
	    if (!strcmp (rsprec.r_id[roffset].sys, tosite) && !(sendhim & SENDHIS))
		continue;				/* written there */
	    if (!strcmp (rsprec.r_from[roffset], tosite) && !(sendhim & SENDHIS))
		continue;				/* came through there */
	    if ((rsprec.r_stat[roffset] & FRMNEWS) && !(sendhim & SENDNEWS))
		continue;				/* never forward NEWS(I) stuff */
	    dmprsp (&io, &note, num, zfile, respnum, NODETAIL, proto);
	    rcount++;
	    io.nrspxmit++;				/* and global stats */
	}
    }
    fclose (zfile);					/* and the dumping file */
    return (ncount + rcount);
}

/*
 *	nfsendone fixes up the sequencer entry for the system/notesfile
 *	pair. The sequencer is only updated if the transmission
 *	was successful (as determined by the retval parameter.
 *
 *	Thanks to Malcolm Slaney for this one.
 */

nfsendone (nfname, tosite, queuestat, sendstat)
char   *tosite;						/* Name of site */
char   *nfname;						/* Name of notesfile */
int     queuestat;					/* Unix return value */
{

/*
 *	Traffic is non-zero if something happened in that notesfile
 *	If nothing at all happened, we save the bother of updating
 *	since it doesn't save us any scanning time later.
 *	(and not updating does save us a little time now)
 *
 *	We check queuestat because we don't want to update the
 *	sequencer file if we know that the transmission failed.
 */
    if (traffic && (!queuestat))			/* only if non-idle */
    {
	fixlast (&entered, io.nf, NORMSEQ, Seqname);	/* update sequencer */
    }

/*
 *	Update statistics only on successful non-empty transmissions.
 *	We don't do any statistics for empty transmissions
 */
#ifdef	STATS
    if ((!queuestat) && (sendstat > 0))			/* if it worked */
    {							/* and sent */
	locknf (&io, DSCRLOCK);
	getdscr (&io, &io.descr);
	gettime (&io.descr.d_lstxmit);			/* mark as sent now */
	if (ncount + rcount > 0)			/* only if sent stuff */
	    io.descr.netwrkouts++;			/* increment xmits */
	putdscr (&io, &io.descr);
	unlocknf (&io, DSCRLOCK);
    }
#endif	STATS

    if (opened)						/* did init(&io,nf) */
	finish (&io);					/* so close it */
/*
 *	Now log everything that happened
 */

    sprdate (&io.stime, line);
    sprintf (fn, "%s/%s/%s", Mstdir, UTILITY, NETLOG);	/* logging */
    if (sendstat >= 0)					/* no bizarre errors */
    {
	if (ncount + rcount)				/* log only if sending */
	{
	    x ((log = fopen (fn, "a")) == NULL, "nfsend: bad net log file");
	    fprintf (log, "%s: %s send (%3d,%3d) to %-10s at %s\n",
		    nfname, queuestat ? "Fail" : "Did", ncount, rcount,
		    tosite, sendtime);
	    x (fclose (log) == EOF, "nfsend: trouble fclosing log file");
	    printf ("%-14s: %4s send (%3d,%3d) to %s since %s\n",
		    nfname, queuestat ? "Fail" : "Did", ncount, rcount,
		    tosite, line);
	}
	else
	{
	    printf ("%-14s: No new notes since %s\n", nfname, line);
	}
	fflush (stdout);				/* force it out */

    }

    if (strcmp (scrfile, "") != 0)			/* contains a name */
    {
	unlink (scrfile);				/* don't leave droppings */
    }

}
