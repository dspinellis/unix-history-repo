static char *sccsid = "%W%";

#include "parms.h"
#include "structs.h"
/*
 *	nfsend will send the notes/responses to a new site that
 *	that site has not seen (at least as far as we are concerned)
 *	This means that we send them the ones that we received later
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

nfsend (tosite, nfname, dmpfile, usetime, atime, sendnews)
char   *tosite,		/* name of site we are sending to */
       *nfname,		/* name of notefile sending to them */
       *dmpfile;	/* and name of local file to place articles in */
struct when_f  *atime;		/* for explicitly specified times */
{
    struct io_f io;
    struct note_f   note;
    struct resp_f   rsprec;
    int     ncount,
            rcount,			/* how many of each sent */
            num,
            rrecnum,
            roffset,
            respnum;
    FILE * log,
	*zfile,
	*fopen ();			/* logfile stuff */
    struct when_f   entered;
    char    seqname[SYSSZ + 20];	/* buffer for sequencer name */
    char    line[DATELEN];		/* hold a formatted date */
    char    fn[WDLEN];			/* path to network log */
    char    sendtime[DATELEN];		/* time we are doing this at */
    struct when_f   xsendtime;		/* unformatted of above */

    gettime (&xsendtime);
    sprdate (&xsendtime, sendtime);	/* format when we sent it */

    x ((zfile = fopen (dmpfile, "w")) == NULL, "nfsend: scratch");

    if (init (&io, nfname) < 0) {
	printf ("%s: no such notesfile: %s\n",SYSTEM,nfname);
	fclose (zfile);
	return(-1);
    }
    if (allow (&io, DRCTOK) == 0) {			/* must be director */
	printf ("%s: not a director of %s\n", SYSTEM, nfname);
	fclose (zfile);
	finish (&io);
	return(-1);
    }
    getperms (&io, 1, tosite);			/* grab system name */
    if (allow (&io, READOK) == 0) {		/* is he allowed to read ?? */
	printf ("%s: site %s has no read permission for %s\n",
		SYSTEM, tosite, nfname);
	finish (&io);					/* close it */
	fclose (zfile);
	return(-1);					/* and go back */
    }

    if ((io.descr.d_stat & NETWRKD) == 0) {
	printf ("%s: notefile %s is not networked\n", SYSTEM, nfname);
	finish (&io);
	fclose (zfile);
	return(-1);
    }


    gettime (&entered);		/* get time for updating sequencer */


    sprintf (seqname, "Sy:%s", tosite);		/* make sequencer name */
    getlast (&io.stime, nfname, 1, seqname);	/* grab the time */
    if (usetime) {
	copydate (atime, &io.stime);		/* use this one instead */
    }
    sprdate (&io.stime, line);
    printf ("%s: %s: Sending articles more recent than %s to %s\n",SYSTEM, nfname, line, tosite);
    rcount = ncount = 0;			/* how many things sent */
    num = 0;					/* start at the beginning */
    while ((num = nxtnote (&io, num, &io.stime)) != -1) {
	getnrec (&io, num, &note);			/* grab the header */
	if (strcmp (note.n_from, tosite) == 0) {
	    goto doresps;				/* they sent it to us */
	}
	if (strcmp (note.n_id.sys, tosite) == 0) {
	    goto doresps;				/* written there */
	}
	if (inorder (&io.stime, &note.n_rcvd) == 0) {
	    goto doresps;			/* just modified since then */
	}
	if ((note.n_stat & FRMNEWS) && (sendnews == 0)) { /* DO NOT send news */
	    goto doresps;				/* cause uniqid probs */
	}
						/* they should have copy */
	dmpnote(&io, &note, num, zfile, NODETAIL);     /* dump to output */
	ncount++;			/* bump count of sent articles */
	io.nnotxmit++;			/* and global stats */

doresps: 				/* process the responses */
	respnum = 0;
	while ((respnum = nxtresp (&io, num, respnum, &io.stime)) != -1) {
	    if (lrsp(&io, num, respnum, &rsprec, &roffset, &rrecnum) == -1)
		break;					/* no response */
	    if (strcmp(rsprec.r_id[roffset].sys, tosite) == 0) {
		continue;				/* written over there */
	    }
	    if (strcmp(rsprec.r_from[roffset], tosite) == 0) {
		continue;		/* came from over there */
	    }
	    if ((rsprec.r_stat[roffset] & FRMNEWS) && (sendnews == 0)) {
		continue;		/* never forward NEWS(I) stuff */
	    }
	    dmprsp(&io, &note, num, zfile, respnum, NODETAIL);
	    rcount++;
	    io.nrspxmit++;				/* and global stats */
	}
    }
    fixlast (&entered, nfname, 1, seqname);	/* update the sequencer time */

    lock(&io, 'n');

    getdscr(&io, &io.descr);
    gettime(&io.descr.d_lstxmit);			/* mark as sent now */
    if (ncount + rcount > 0) {			/* only if sent stuff */
	io.descr.netwrkouts++;			/* increment number of xmits */
    }
    putdscr(&io, &io.descr);

    unlock(&io, 'n');

    finish(&io);				/* close the notefile */
    fclose(zfile);				/* and the dumping file */
    printf("%s: %s: %d notes & %d responses sent to %s\n",
	    SYSTEM, nfname, ncount, rcount, tosite);
    sprintf(fn, "%s/%s/%s", MSTDIR, UTILITY, NETLOG);
    if (ncount + rcount) {			/* log only if sending */

	glock(&io, LOGLOCK);			/* now, update the log file */
	x ((log = fopen(fn, "a")) == NULL, "nfsend: bad net log file");

	fprintf(log, "%s: sent %d notes & %d responses to %s at %s\n",
		nfname, ncount, rcount, tosite, sendtime);
	x (fclose(log) == EOF, "nfsend: trouble fclosing log file");
	gunlock(&io, LOGLOCK);				/* and unlock */
    }
    return(ncount+rcount);
}
