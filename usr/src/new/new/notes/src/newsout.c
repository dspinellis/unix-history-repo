static char *sccsid = "@(#)newsout.c	1.2\t1/24/83";

#include "parms.h"
#include "structs.h"
#include "newsgate.h"
/*
 *	newsoutput - process a particular notesfile for updates
 *	out to the news system.
 *
 *	This particular implementation of the program DOES NOT 
 *	recombine split articles. This is a shame, but I wanted a
 *	fast implementation.
 *
 *	Original Coding:	Ray Essick	April 1982
 */

newsout (nfname, othersok)
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
    char    ngroup[NNLEN];				/* hold newsgroup */
    FILE * log;
    char    buf[CMDLEN];
    char    ztime[DATELEN];

    if (init(&io, nfname) < 0)	{		/* open the bugger */
	printf("Can not open notesfile `%s'\n", nfname);
	return(-1);
    }
    if ((io.descr.d_stat & NETWRKD) == 0) {	/* are we permitted to gate? */
	closenf(&io);
	printf("%s must be networked to go to news!\n", nfname);
	return(-1);
    }

    gettime(&whendump);			/* for updating last access */
    getlast(&io.stime, nfname, 1, NEWSSYS);	/* grab last time dumped */
    newsgroup(io.nf, ngroup, NFNEWS);		/* alias 'newsgroup' */

    ndumped = rdumped = 0;
    notenum = 0;				/* start at the top */
    while ((notenum = nxtnote(&io, notenum, &io.stime)) != -1) {
	getnrec (&io, notenum, &note);		/* get descriptor */
	respnum = 0;				/* response chain */
	if (inorder (&io.stime, &note.n_rcvd) == 0) {     /* been dumped */
	    goto doresps;
	}
	if ((note.n_stat & FRMNEWS) != 0) {
	    goto doresps;			/* stupid to send it back! */
	}
	if ((note.n_stat & ORPHND)!=0) {
	    goto doresps;	/* don't send foster dad out */
	}
	if ((strcmp (note.n_id.sys, SYSTEM) != 0) && (othersok == 0)) {
	    goto doresps;			/* don't dump non-local */
	}

	if (newsnote (&io, &note, notenum, ngroup) == -1) {	/* dump it */
	    sprintf(buf, "%s/%s/%s", MSTDIR, UTILITY, NETLOG);
	    sprdate(&whendump, ztime);
	    glock(&io, LOGLOCK);			/* mutual exclusion */
	    x ((log = fopen (buf, "a")) == NULL, "newsout: missing log file");
	    fprintf(log, "%s: failed to send to NEWS at %s\n",
		    nfname, ndumped, rdumped, ztime);
	    fclose(log);
	    gunlock(&io, LOGLOCK);				/* all done */
	}
	ndumped++;					/* count */
	sleep (2);					/* let news run ... */

doresps: 					/* let's process responses */


	while ((respnum = nxtresp(&io, notenum, respnum, &io.stime)) != -1) {
	    if (lrsp (&io, notenum, respnum, &rsprec, &roffset, &rblock) == -1)
		break;					/* bad chain */
	    if (rsprec.r_stat[roffset] & FRMNEWS)
		continue;				/* never back to news */
	    if ((strcmp(rsprec.r_id[roffset].sys, SYSTEM) != 0) && (othersok == 0))
		continue;			/* don't dump non-local */

	    if (newsresp(&io, &note, notenum, &rsprec, roffset, respnum, ngroup) == -1) {
		sprintf(buf, "%s/%s/%s", MSTDIR, UTILITY, NETLOG);
		sprdate(&whendump, ztime);
		glock(&io, LOGLOCK);			/* mutual exclusion */
		x ((log = fopen (buf, "a")) == NULL, "newsout: missing log file");
		fprintf(log, "%s: failed to send to NEWS at %s\n",
			nfname, ndumped, rdumped, ztime);
		fclose(log);
		gunlock(&io, LOGLOCK);				/* all done */
	    }
	    sleep (2);					/* let news run ... */
	    rdumped++;
	}
    }

    fixlast(&whendump, nfname, 1, NEWSSYS);		/* update sequencer */

    if (ndumped + rdumped) {				/* log dump */
	sprintf(buf, "%s/%s/%s", MSTDIR, UTILITY, NETLOG);
	sprdate(&whendump, ztime);
	glock(&io, LOGLOCK);			/* mutual exclusion */
	x ((log = fopen (buf, "a")) == NULL, "newsout: missing log file");
	x ((log = fopen (buf, "a")) == NULL, "newsout: missing log file");
	fprintf(log, "%s: sent %d notes & %d responses to NEWS at %s\n",
		nfname, ndumped, rdumped, ztime);
	fclose(log);
	gunlock(&io, LOGLOCK);				/* all done */
    }

    finish(&io);					/* close shop here */

    return(0);
}
