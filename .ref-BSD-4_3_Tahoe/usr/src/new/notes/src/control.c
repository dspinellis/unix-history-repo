#include	"parms.h"
#include	"structs.h"

#ifdef	FASTSEQ
#include	<sys/types.h>
#include	<sys/stat.h>
#endif	FASTSEQ

#ifdef	RCSIDENT
static char rcsid[] = "$Header: control.c,v 1.7 85/01/18 15:07:18 notes Rel $";
#endif	RCSIDENT

/*
 *	Code in this module ( control.c) is designed to process
 *	a single notefile. Its paramters include the name of
 *	the notefile to process and whether to use the sequencer.
 *
 *	Original author: Rob Kolstad	Winter, 1980.
 *	Modifications:	Ray Essick	December, 1981.
 *
 */


control (nfname, seqon)
char   *nfname;
{
    struct io_f io;
    struct note_f   note;
    struct when_f   entered;				/* entry time */

    int     firstdis,
            lastdis;					/* display counters */
    int     readnum;					/* start reading */
    int     respnum;					/* response to be on */
							/* currently 0 always */

    if (nfname == NULL || nfname[0] == '\0')		/* a name? */
	return (-1);					/* silly boy */

    if (intflag)					/* did he hit DEL? */
	return (QUITFAST);				/* prejudiced quit */

#ifdef	FASTSEQ
    if (seqon != NOSEQ && seqon != EXTSEQ)		/* if sequencing */
    {
	struct when_f   whenvec;
	char    NoteFile[WDLEN];
	struct stat StatBuf;

	if (nfname[0] == '/')				/* absolute pathname */
	{
	    getlast (&io.stime, rindex (nfname, '/') + 1, seqon, Seqname);
	    sprintf (NoteFile, "%s/%s", nfname, TEXT);
	}
	else						/* relative to Mstdir */
	{
	    getlast (&io.stime, nfname, seqon, Seqname);
	    sprintf (NoteFile, "%s/%s/%s", Mstdir, nfname, TEXT);
	}
	if (stat (NoteFile, &StatBuf) >= 0)
	{
	    maketime (&whenvec, (long) StatBuf.st_mtime);
	    if (inorder (&whenvec, &io.stime))
	    {
		putchar ('\r');				/* from Brian Redman */
		ceol ();				/* ... harpo!ber */
		printf ("%s...", nfname);
		fflush (stdout);
		return (0);
	    }
	}
    }
#endif	FASTSEQ

    gettime (&entered);					/* get the entry time */
    if (init (&io, nfname) < 0)
    {
	fflush (stdout);				/* get it out */
	sleep (1);					/* let him read it */
	return (-1);
    }
    if (allow (&io, READOK) == 0)
    {
	if (seqon == NOSEQ)				/* skip if sequencing */
	{
	    printf ("You aren't allowed to read %s\n", io.fullname);
	    if (io.descr.d_plcy)
	    {
		if (askyn ("Do you wish to see the policy note (y/n)? ") == 'y')
		{
		    getnrec (&io, 0, &note);		/* show the policy */
		    dspnote (&io, &note, 0);
		}
	    }
	    else
	    {
		printf ("There is no policy note");
	    }
	    if (allow (&io, WRITOK))
	    {
		printf ("\nYou may leave a note in the notefile\n");
		if (askyn ("Do you wish to leave a note (y/n) ?") == 'y')
		{
		    if (addnote (&io, NULL, "Edit Note Text:", "Note title: ", NULL, EDIT) != -1)
			printf ("Your note has been registered\n");
		}
	    }
	    else
	    {
		printf ("\nHit any key to continue");
		gchar ();				/* grab and drop it */
		sleep (2);				/* let him read it */
	    }
	}
	finish (&io);
	return (-1);
    }

    if ((io.descr.d_stat & OPEN) == 0)
    {
	printf ("Notefile %s is closed\n", nfname);
	fflush (stdout);
	sleep (1);
    }

    if (io.descr.d_stat & OPEN || allow (&io, DRCTOK))
    {
	getlast (&io.stime, io.nf, seqon, Seqname);	/* last time here */
	firstdis = io.descr.d_nnote - Nindex + 1;
	respnum = 0;					/* go to base note */

	if (inorder (&io.descr.d_lastm, &io.stime) &&
		(seqon != EXTSEQ))			/* always enter mode */
/*	    if sequencer is off, stime is jan 1 1970, so will enter notefile */
	{
	    putchar ('\r');				/* from Brian Redman */
	    ceol ();					/* ... harpo!ber */
	    printf ("%s...", nfname);
	    fflush (stdout);				/* force it out */
#ifdef	FASTSEQ
/*
 *	Update his timestamp here so that he will fall through the
 *	fast processing loop next time. Thanks to Rob Kolstad for
 *	thinking this through.
 */
	    fixlast (&entered, io.nf, seqon, Seqname);
#endif	FASTSEQ
	    closenf (&io);				/* close nicely */
	    return 0;					/* "normal" quit */
	}
	else						/* enter the notesfile */
	{
	    if (seqon != INDXSEQ && seqon != NOSEQ)	/* if to note */
	    {
		if ((readnum = nxtnote (&io, 0, &io.stime)) > 0)
		    goto seqenter;
	    }						/* else to index */
	    while (1)
	    {
		prntind (&io, &firstdis, &lastdis);
		if ((readnum = indx (&io, &firstdis, &lastdis, &respnum)) == -1)
		    continue;
		if (readnum < -1)
		    break;

	seqenter: 					/* entry point if sequencer on */
		readnum = readem (&io, readnum, &firstdis, respnum);
		if (readnum < -1)
		    break;
	    }
	    if (readnum == QUITSEQ || readnum == QUITUPD)/* fix his timestamp */
		fixlast (&entered, io.nf, seqon, Seqname);
	    intflag = 0;				/* clean for next */
	}

    }
    finish (&io);

    return readnum;					/* supplied value */
}
