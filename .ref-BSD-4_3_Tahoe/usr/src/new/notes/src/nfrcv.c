#define	MAINLINE

#ifdef	RCSIDENT
static char rcsid[] = "$Header: nfrcv.c,v 1.7.0.1 85/08/04 06:41:51 notes Rel $";
#endif	RCSIDENT

/* 
 *	netrcv - load a notefile from the network
 *	accepts 2 parameters (1) the source system
 *			     (2) the notefile to load into
 *
 *	This program should be called only by the netload program
 *	or a suitably informed user/wizard
 *
 *	Original author:	Ray Essick	december 1981
 */

#include "parms.h"
#include "structs.h"
#include "net.h"

main (argc, argv)
char  **argv;
{
    struct io_f io;
    FILE * log,						/* network logfile */
	*fopen ();
    char    fn[WDLEN];					/* network logfile */
    char    nowtime[DATELEN];
    char    buf[CMDLEN];
    struct when_f   atime;
    int     fid;					/* for close loop */

    setuid (geteuid ());				/* force to notes */
    startup (argc, argv);				/* common init */

/*
 *	make damn sure we have enough file descriptors 
 */
    for (fid = 3; fid < 20; fid++)
	close (fid);

    if (argc != 3)
    {
	printf ("Usage: %s notefile fromsystem\n", argv[0]);
	exit (BAD);
    }

    sprintf (fn, "%s/%s/%s", Mstdir, UTILITY, NETLOG);
    gettime (&atime);
    sprdate (&atime, nowtime);

    if (init (&io, argv[1]) < 0)
    {
#ifdef	AUTOCREATE					/* if making 'em */
	if (argv[1][0] == '/')				/* absolute? */
	{
#ifdef	NFMAINT
	    {
		char    tbuf[128];			/* title */
		sprintf (buf, "%s: non-existent notesfile from %s\nAborted by nfrcv\n",
			argv[1], argv[2]);
		sprintf (tbuf, "Nfrcv ignores %s", argv[1]);
		nfcomment (NFMAINT, buf, tbuf, 0, 0);
	    }
#endif	NFMAINT
	    printf ("Nfrcv: %s: no autocreate for absolute pathnames\n",
		    argv[1]);
	    exit (BAD);					/* abort */
	}
	buildnf (argv[1], Mstdir, 0, 1, 1);		/* no anon, open, networked */
	x (init (&io, argv[1]) < 0, "nfrcv: couldn't create notesfile");
#ifdef	NFMAINT
	{
	    char    tbuf[128];				/* title */
	    sprintf (buf, "%s: non-existent notesfile from %s created by nfrcv\n",
		    argv[1], argv[2]);
	    sprintf (tbuf, "Nfrcv creates %s", argv[1]);
	    nfcomment (NFMAINT, buf, tbuf, TRUE, 0);
	}
#endif	NFMAINT

#else							/* just tell about it */
	x ((log = fopen (fn, "a")) == NULL, "netrcv: bad log open");
	sprintf (buf, "%s: Non-existent notesfile received from %s at %s\n",
		argv[1], argv[2], nowtime);
	fprintf (log, "%s", buf);			/* log it in the log */
	x (fclose (log) == EOF, "netrcv: bad close of log file");
#ifdef	NFMAINT						/* log it in a nf also? */
	nfcomment (NFMAINT, buf, "non-notesfile to nfrcv", 0, 0);
#endif
	exit (NONF);
#endif	AUTOCREATE					/* of autocreate else clause */
    }

    if ((io.descr.d_stat & NETWRKD) == 0)
    {
	printf ("%s is not a networked notefile on %s\n", argv[1], System);
	finish (&io);
	x ((log = fopen (fn, "a")) == NULL, "netrcv: bad log open");
	fprintf (log, "%s: Non-networked notesfile received from %s at %s\n",
		argv[1], argv[2], nowtime);
	x (fclose (log) == EOF, "netrcv: bad close of log file");
	exit (NONF);					/* un-networked appears not there */
    }

    loadem (&io, stdin, LOCKIT, argv[2], NODETAIL, -1);

    x ((log = fopen (fn, "a")) == NULL, "netrcv: couldnt open log file");
    fprintf (log, "%s: insert (%d,%d), drop (%d,%d) from %s at %s\n",
	    argv[1], io.nnotrcvd, io.nrsprcvd, io.nnotdrop, io.nrspdrop,
	    argv[2], nowtime);
    x (fclose (log) == EOF, "netrcv: bad close of log file");

#ifdef	STATS
    locknf (&io, DSCRLOCK);				/* bump count of netowrk calls rcvd */
    getdscr (&io, &io.descr);
    io.descr.netwrkins++;				/* bump the count */
    putdscr (&io, &io.descr);
    unlocknf (&io, DSCRLOCK);
#endif	STATS

    printf ("%s: Inserted: (%d,%d) Dropped: (%d,%d)\n",
	    argv[1], io.nnotrcvd, io.nrsprcvd, io.nnotdrop, io.nrspdrop);

    finish (&io);
    exit (GOOD);
}
