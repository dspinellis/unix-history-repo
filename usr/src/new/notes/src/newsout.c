#include "parms.h"
#include "structs.h"
#include "newsgate.h"

#ifdef	FASTSEQ
#include	<sys/types.h>
#include	<sys/stat.h>
#endif	FASTSEQ

#ifdef	RCSIDENT
static char rcsid[] = "$Header: newsout.c,v 1.7.0.2 85/03/22 10:53:59 notes Rel $";
#endif	RCSIDENT

/*
 *	newsoutput - process a particular notesfile for updates
 *	out to the news system.
 *
 *
 *	Original Coding:	Ray Essick	April 1982
 *	Modified to handle gateing for multiple systems better
 *				Ray Essick	September 1982
 */

newsout (nfname, backwards, usetime, verbosity)
char   *nfname;
{
    struct io_f io;
    struct note_f   note;
    struct resp_f   rsprec;
    struct when_f   whendump;				/* when we did this */
    int     notenum,
            respnum,
            rdumped,
            ndumped,					/* number dumped */
            roffset,
            rblock;
    char    basengroup[NNLEN];				/* hold newsgroup */
    char    respngroup[NNLEN];				/* hold newsgroup */
    FILE * log;
    char    buf[CMDLEN];
    char    ztime[DATELEN];

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
	else						/* from Mstdir */
	{
	    getlast (&io.stime, nfname, usetime, Seqname);
	    sprintf (NoteFile, "%s/%s/%s", Mstdir, nfname, TEXT);
	}
	if (stat (NoteFile, &StatBuf) >= 0)
	{
	    maketime (&whenvec, (long) StatBuf.st_mtime);
	    if (inorder (&whenvec, &io.stime))
	    {
		return (0);
	    }
	}
    }
#endif	FASTSEQ

    if (init (&io, nfname) < 0)				/* open the bugger */
	return (-1);

    if ((io.descr.d_stat & NETWRKD) == 0)		/* can we gate? */
    {
	closenf (&io);
	printf ("%s must be networked to go to news!\n", nfname);
	fflush (stdout);
	return (-1);
    }

    gettime (&whendump);				/* for seq. update */
    getlast (&io.stime, io.nf, usetime, Seqname);	/* grab last time */
    newsgroup (io.nf, basengroup, NFBASENEWS);		/* alias base notes */
    newsgroup (io.nf, respngroup, NFRESPNEWS);		/* and responses */

    if (inorder (&io.descr.d_lastm, &io.stime))		/* no traffic */
    {
#ifdef	FASTSEQ
/*
 *	Update the timestamp so the next scan will catch the idle
 *	notesfile in the FASTSEQ code.
 */
	fixlast (&whendump, io.nf, NORMSEQ, Seqname);	/* update sequencer */
#endif	FASTSEQ
	closenf (&io);					/* cheap no-stats exit */
	return (0);					/* and out of here */
    }

    ndumped = rdumped = 0;
    notenum = 0;					/* start at the top */
    while ((notenum = nxtnote (&io, notenum, &io.stime)) != -1)
    {
	getnrec (&io, notenum, &note);			/* get descriptor */
	respnum = 0;					/* response chain */
	if (inorder (&io.stime, &note.n_rcvd) == 0)	/* been dumped */
	    goto doresps;
	if ((note.n_stat & FRMNEWS) != 0)		/* it's been in news */
	    goto doresps;				/* dont send back! */
	if ((note.n_stat & ORPHND) != 0)		/* no foster parents */
	    goto doresps;				/* go out */
	if (!cansend (note.n_id.sys, sendclass))	/* can't send it */
	    goto doresps;				/* don't dump it */

	if (newsnote (&io, &note, notenum, basengroup, backwards) == -1)
	{
	    sprintf (buf, "%s/%s/%s", Mstdir, UTILITY, NETLOG);
	    sprdate (&whendump, ztime);
	    x ((log = fopen (buf, "a")) == NULL, "newsout: no log file");
	    fprintf (log, "%s: Failed dumping note to NEWS for %s at %s\n",
		    nfname, note.n_id.sys, ztime);
	    fclose (log);
	    printf ("%s: Failed dumping note to NEWS for %s at %s\n",
		    nfname, note.n_id.sys, ztime);
	    fflush (stdout);
	}
							/* dump it */
	ndumped++;					/* count */

doresps: 						/* process responses */


	while ((respnum = nxtresp (&io, notenum, respnum, &io.stime)) != -1)
	{
	    if (lrsp (&io, notenum, respnum, &rsprec, &roffset, &rblock) == -1)
		break;					/* bad chain */
	    if (rsprec.r_stat[roffset] & FRMNEWS)	/* its from there */
		continue;				/* dont go back */
	    if (!cansend (rsprec.r_id[roffset].sys, sendclass))/* if can't then */
		continue;				/* don't send it */

	    if (newsresp (&io, &note, notenum, &rsprec, roffset,
			respnum, respngroup, backwards) == -1)
	    {
		sprintf (buf, "%s/%s/%s", Mstdir, UTILITY, NETLOG);
		sprdate (&whendump, ztime);
		x ((log = fopen (buf, "a")) == NULL, "newsout: no log file");
		fprintf (log, "%s: Failed dumping note to NEWS for %s at %s\n",
			nfname, rsprec.r_id[roffset].sys, ztime);
		fclose (log);
		printf ("%s: Failed dumping note to NEWS for %s at %s\n",
			nfname, rsprec.r_id[roffset].sys, ztime);
		fflush (stdout);
	    }
	    rdumped++;


	}
    }

/*
 *	update the sequencer always.  This is fine if we did send
 *	something for the system (and what we want to happen).
 *	By updating even when we don't send news, we avoid having
 *	to rescan those candidates we just looked at the next time.
 *	Eg: Running newsoutput for a usually quiet site could get
 *	very expensive if we didn't update this timestamp.
 *
 *	We catch the "idle notesfile" case earlier and leave
 *	without scanning or updating if nothing of potential
 *	interest has happened since the last run.
 */
    fixlast (&whendump, io.nf, NORMSEQ, Seqname);	/* update sequencer */

    if (ndumped + rdumped)				/* log dump */
    {
	sprintf (buf, "%s/%s/%s", Mstdir, UTILITY, NETLOG);
	sprdate (&whendump, ztime);
	x ((log = fopen (buf, "a")) == NULL, "newsout: missing log file");
	fprintf (log, "%s: Send (%d,%d) to NEWS at %s\n",
		nfname, ndumped, rdumped, ztime);
	fclose (log);
	printf ("%s: Send (%d,%d) to NEWS (%s and %s) at %s\n",
		nfname, ndumped, rdumped,
		basengroup, respngroup, ztime);
	fflush (stdout);
    }

    finish (&io);					/* close shop here */
    return 0;
}

/*
 *	cansend(system,sendclass)
 *
 *	check if we are gatewaying articles for the system named
 *	in "whichsys".  Use the "sendclass" variable to check
 *	against sending anyone (NEWS_ALLSEND bit)
 *	If that doesn't qualify us, look through a list of
 *	sitenames in the gatesysname[] array.
 */

cansend (sysname, class)
char   *sysname;
int     class;
{
    int     i,
            j,
            k;
    extern int  gatesyscount;				/* size of array */
    extern char *gatesysnames[GATEMAX];			/* actual data */

    if (class & NEWS_ALLSEND)
	return (1);					/* sending any articles */

    /* 
     * This list should be sorted and we should do a binary search
     * on the bugger....
     */
    for (i = 0; i < gatesyscount; i++)			/* look through table */
	if (!strcmp (sysname, gatesysnames[i]))
	    return (1);					/* find and dandy */

    return (0);						/* anything else is nogo */
}
