#define	MAINLINE

#ifdef	RCSIDENT
static char rcsid[] = "$Header: nfxmit.c,v 1.7 85/01/18 15:31:53 notes Rel $";
#endif	RCSIDENT

/*
 *	This program (netsend) will parse off the control card a destination 
 *	site, an optional date for sequencing purposes, and then a list
 *	of notefiles. These notefiles are then scanned to send new items
 *	to the destination site. The entire arguement list is scanned once
 *	to pull in the switches and then again to dump the notefiles.
 *	This implies that the switches are indeed global, and that the last
 *	specified ones are the ones which are used.
 *
 *	Original Coding:	Ray Essick	December 1981
 */
#include "parms.h"
#include "structs.h"
#include "net.h"

static int  usetime;					/* whether overriding sequencer time */
static int  sendhim;					/* classes to send */
static int  callback;					/* whether to have remote send back */
static char tosite[SYSSZ + 20];				/* with some buffer space */
static char dmpfile[WDLEN];				/* a scratch file name */

xmitone (local)
char   *local;
{
    char    cmdline[CMDLEN];				/* build your favorite command */
    char    buf[CMDLEN];
    char    nfname[NNLEN];				/* hold aliased nf name */
    char   *xmit,
           *rply;					/* for non-standard */
    int     proto;					/* protocol to use */
    int     queuestat;					/* Return Value */
    int     sendstat;					/* nfsend status */

    getnet (tosite, &xmit, &rply, &proto);		/* see if non standard */
    nfalias (local, nfname, tosite);			/* get remote file name */

    if (callback)
    {
	if (rply == NULL)
	    sprintf (cmdline, DFLTRPLY,			/* see net.h defn */
		    tosite, NFXMIT, nfname, System);
	else
	    sprintf (cmdline, rply, nfname, System);	/* do his */
#ifndef	FASTFORK
	dounix (cmdline, 0, 0);				/* do it */
#else
	dounix (0, 0, hisshell, "-c", cmdline, 0, 0);	/* let shell interpret */
#endif	FASTFORK
    }

/*
 *	Now see if we have anything to send him.  We don't queue anything
 *	unless we do want to send to him.
 */

    if ((sendstat =					/* WANT ASSIGN */
		nfsend (tosite, local, dmpfile, usetime, sendhim, proto)) > 0)
    {
	if (xmit == NULL)
	    sprintf (cmdline, DFLTXMIT,			/* see net.h defn */
		    tosite, NFRCV, nfname, System, dmpfile);
	else
	{
	    sprintf (buf, xmit, nfname, System);
	    sprintf (cmdline, "%s < %s", buf, dmpfile);	/* feed stdin to it */
	}
#ifndef	FASTFORK
	queuestat = dounix (cmdline, 0, 0);		/* do it */
#else
	queuestat = dounix (0, 0, hisshell, "-c", cmdline, 0, 0);
#endif FASTFORK
    }
    else
    {							/* error or no data */
	queuestat = 0;					/* avoid random stack noise */
    }

/*
 *	nfsendone cleans up the sequencer entry for that notesfile.
 *	And does logging information also
 *	We only clean up the entry if the command was successful.
 */
    nfsendone (local, tosite, queuestat, sendstat);

    return (0);						/* all is well */
}

main (argc, argv)
char  **argv;
{
    int     i;
    struct nflist_f *nfptr;
    char    fmtdate[DATELEN];				/* formatted date */
    struct when_f   ztime;				/* hold date */

    startup (argc, argv);				/* common init */

    if (argc == 1)
    {
	fprintf (stderr, "Usage: %s -d<site> [-r] [-i] [-a] [-t datespec]  [-f file] nf [nf2 ..]\n", argv[0]);
	exit (BAD);
    }
    sendhim = 0;					/* default classes */
    usetime = NORMSEQ;					/* sequencer time */
    callback = 0;					/* no return messages */
    sprintf (tosite, "*None*");				/* null site */

    for (i = 1; i < argc; i++)				/* parse options */
	switch (argv[i][0])
	{
	    case '-': 					/* some options oh goody */
		switch (argv[i][1])
		{
		    case 'd': 
			if (strmove (argv[i] + 2, tosite) > SYSSZ)
			{
			    printf ("System name: %s, too long\n", argv[i] + 2);
			    exit (BAD);
			}
			break;				/* out of this switch statement */

		    case 'r': 				/* force rmt nfxmit */
			callback = 1;
			break;

		    case 'a': 				/* ok, news articles */
			sendhim |= SENDNEWS;
			break;

		    case 'i': 				/* stuff he's seen */
			sendhim |= SENDHIS;
			break;

		    case 't': 				/* explicit time */
		    case 'o': 				/* compatibility */
			if (++i == argc)
			{
			    fprintf (stderr, "-t option requires following date\n");
			    exit (BAD);
			}
			switch (parsetime (argv[i], &ztime))
			{
			    case 0: 			/* ok */
				usetime = BASESEQ;	/* use this time */
				Basetime = ztime;	/* store it */
				sprdate (&ztime, fmtdate);/* format */
				printf ("%s: Sending articles since %s\n",
					Invokedas, fmtdate);
				break;
			    case -1: 			/* no good */
				fprintf (stderr, "%s: unable to parse time `%s'\n",
					Invokedas, argv[i]);
				exit (BAD);
			    case -2: 			/* in future */
				fprintf (stderr, "%s: parsed date (%s) is in the future\n",
					Invokedas, argv[i]);
				exit (BAD);
			}
			break;

		    case 'f': 				/* next list is a file name */
			if (++i == argc)		/* no filename */
			{
			    fprintf (stderr, "-f must be followed by filename\n");
			    exit (BAD);
			}
			readrc (argv[i]);		/* load it */
			break;

		    default: 
			printf ("Bad switch '%c'\n", argv[i][1]);
			exit (BAD);
		}
		break;

	    default: 					/* a notefile name */
		expand (argv[i]);			/* add it to the list */
		break;
	}

    sprintf (dmpfile, "/tmp/nfxmit%d", getpid ());
    if (strcmp ("*None*", tosite) == 0)
    {
	printf ("Null destination - use -d flag\n");
	exit (BAD);
    }

/*	now that we have processed all the parameters, lets dump the
 *	notes and send them to the other people.
 *	This is a 2 step process. First we make a file and then
 *	we 'uucp' it to the other site.
 */

    while ((nfptr = nextgroup ()) != (struct nflist_f *) NULL)
	xmitone (nfptr -> nf_name);			/* simple case */

    exit (GOOD);
}
