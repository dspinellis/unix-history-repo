#define	MAINLINE
#include "parms.h"
#include "structs.h"
#include "newsgate.h"

#ifdef	RCSIDENT
static char rcsid[] = "$Header: newsoutput.c,v 1.7.0.1 85/03/22 10:54:22 notes Rel $";
#endif	RCSIDENT


/*
 *	newsoutput - place the specified notesfiles out to the
 *	news susbsystem. 
 *
 *	Original Coding:	Ray Essick	April 1982
 *	Modified to better handle multiple system gateing
 *				Ray Essick	September 1982
 */

static char *whichsys;					/* who dumping for */
static int  verbosity = 0;				/* quiet */
static int  backward = 0;				/* do old format */
char    rnewscmd[CMDLEN] = DFLTRNEWS;			/* how to rnews */
int     sendclass = 0;					/* classes to send */
int     usetime = NORMSEQ;				/* user supplied */
int     gatesyscount = 0;				/* actives slots in */
char   *gatesysnames[GATEMAX];				/* list of names */

newsone (nfname)
char   *nfname;
{
    if (verbosity)
    {
	printf ("\t%s\n", nfname);
	fflush (stdout);
    }
    return newsout (nfname, backward, usetime, verbosity);
}


main (argc, argv)
char  **argv;
{
    int     i;
    struct nflist_f *nfptr;
    char   *outgoing;
    struct when_f   ztime;
    char    fmtdate[DATELEN];				/* formatted date */
    int     aflag,
            cflag,
            sflag;

    startup (argc, argv);				/* common init */

    if (argc == 1)
    {
	printf ("Usage: %s [-ssitename | -a] [-v] [-b] [-f file] topic1 [topic2 ...]\n", argv[0]);
	exit (BAD);
    }

    if (globuid != Notesuid)
    {
	printf ("Sorry, only notes 'owner' can send notes to news\n");
	exit (BAD);
    }

    whichsys = System;					/* defaults to local */
    aflag = cflag = sflag = 0;				/* none done */
    gatesyscount = 0;					/* none there */
    verbosity = 0;					/* no messages */
    backward = 0;					/* just new format */
    usetime = NORMSEQ;					/* non-seq time */
    strcpy (rnewscmd, DFLTRNEWS);			/* default rnews */
    getnet (RNEWSNAME, &outgoing, NULL, NULL);		/* how to get there */
    if (outgoing != (char *) NULL)			/* specified? */
	strcpy (rnewscmd, outgoing);			/* load it */

    for (i = 1; i < argc; i++)
    {
	if (argv[i][0] == '-')
	    switch (argv[i][1])
	    {

		case 's': 				/* gateway for */
		    sflag = 1;
		    if (argv[i][2] != '\0')		/* -ssite */
		    {
			whichsys = &argv[i][2];
		    }
		    else
		    {					/* -s sitename */
			if (++i == argc)		/* no sitename */
			{
			    fprintf (stderr, "-s must be followed by a sitename\n");
			    exit (BAD);
			}
			whichsys = argv[i];		/* mark site */
		    }
		    gatesyscount = 1;			/* fill in table */
		    gatesysnames[0] = whichsys;
		    break;

		case 'a': 				/* all non-local */
		    sendclass |= NEWS_ALLSEND;
		    aflag = 1;
		    whichsys = NEWSSYS;			/* it's sequencer */
		    break;

		case 'c': 				/* file of systems */
		    cflag = 1;
		    whichsys = NEWSSYS;			/* use the global seq */
		    if (++i == argc)
		    {
			printf ("%s: -c option must be followed by filename\n", Invokedas);
			exit (BAD);
		    }
		    loadgate (argv[i]);			/* load the table */
		    break;

		case 'v': 				/* messages per group */
		    verbosity++;
		    break;

		case 'f': 				/* process a file */
		    if (++i == argc)			/* no filename */
		    {
			fprintf (stderr, "-f must be followed by a filename\n");
			exit (BAD);
		    }
		    readrc (argv[i]);			/* read the file */
		    break;

		case 'b': 				/* include old headers */
		    backward++;
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
			    usetime = BASESEQ;		/* use this time */
			    Basetime = ztime;		/* store it */
			    sprdate (&ztime, fmtdate);	/* format */
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


		default: 
		    printf ("Bad switch: %c\n", argv[i][1]);
		    exit (BAD);
	    }
	else
	    expand (argv[i]);				/* put it in the list */
    }

/*
 *	now, run down the list and send each notesfile.
 *	check to make sure some things are used consistently first.
 */

    if (aflag + cflag + sflag > 1)
    {							/* -a and -s */
	fprintf (stderr, "%s: -a, -c and -s mutually exclusive\n", Invokedas);
	exit (BAD);
    }
    if (gatesyscount == 0 && !cflag)			/* default to local */
    {
	/* 
	 * check against cflag allows empty -c files to "work"
	 */
	gatesysnames[0] = whichsys;
	gatesyscount = 1;				/* just us */
    }
    if (1)						/* was verbosity */
    {							/* now always... */
	printf ("%s: Sending news articles through command: %s\n",
		Invokedas, rnewscmd);
	if (sendclass & NEWS_ALLSEND)
	{
	    printf ("Sending to News for any system\n");
	}
	else
	{
	    int     col;				/* count columns */

	    /* 
	     * WE WANT TO SORT THE LIST AT THIS POINT
	     * so that the cansend() routine in newsout.c can do a binary
	     * search to make things faster.
	     */
	    printf ("Gateway for the following %d system(s):\n", gatesyscount);
	    for (i = 0, col = 0; i < gatesyscount; i++)
	    {
		printf (" %16s", gatesysnames[i]);
		if (++col % 4 == 0)
		    putc ('\n', stdout);		/* multi-column */
	    }
	}
	printf ("\n================\n");
    }
    fflush (stdout);					/* make sure in order */
    sprintf (Seqname, "%s:%s", NEWSSEQ, whichsys);
    while ((nfptr = nextgroup ()) != (struct nflist_f *) NULL)
	newsone (nfptr -> nf_name);

    exit (GOOD);					/* all done */
}

/*
 *	loadgate(filename)
 *
 *	fill in the system table.
 */

loadgate (filename)
char   *filename;
{
    FILE * fptr;
    char    onesystem[256];				/* system name */

    if ((fptr = fopen (filename, "r")) == (FILE *) NULL)
	return (-1);					/* bah humbug */

    while (fscanf (fptr, "%s", onesystem) == 1)
    {
	gatesysnames[gatesyscount++] = strsave (onesystem);
    }

    fclose (onesystem);
    return (0);
}
