#define		MAINLINE
#include	"parms.h"
#include	"structs.h"

#ifdef	RCSIDENT
static char rcsid[] = "$Header: main.c,v 1.7.0.1 85/03/08 10:40:05 notes Rel $";
#endif	RCSIDENT

/*
 *	this  main program will take in a notefile name, and
 *	then proceed to handle all of the processing for it. This includes
 *	calling all of the appropriate routines. It should stay in the 
 *	package pretty much as is. It may not be the master routine, but
 *	it will be the chief 'driver' while within a particular note.
 *
 *	Original author: Rob Kolstad	Winter, 1980.
 *	Modifications:	Ray Essick	June, 1981.
 *	Modified more:	Ray Essick	May, 1982.
 *
 *
 */


static int  seqon = NOSEQ;				/* sequencer mode */

main (argc, argv)
char  **argv;
{

    int     i;
    char   *p,
           *q,
           *calledas;
    int     autoseq = 0;				/* if autoseq */
    int     notesargs = 0;				/* count nf's */
    int     nfsdone;
    struct nflist_f *nfptr;
    char    bufstdout[BUFSIZ];				/* buffer for speed */

    setbuf (stdout, bufstdout);				/* buffer it */
    startup (argc, argv);				/* common init */

    if (globuid == Anonuid)
    {
	fprintf (stderr, "Sorry, you have the wrong uid (%d) to use notesfiles.\n",
		globuid);
	fprintf (stderr, "Consult your local system guru for more help\n");
	exit (BAD);
    }
    if ((calledas = rindex (argv[0], '/')) != 0)	/* get invoking name */
	calledas++;
    else
	calledas = argv[0];				/* no slash */
    if (strcmp (calledas, "notes") && *calledas != '=')	/* autoseq entry */
    {
	autoseq++;					/* is autoseq */
	setseq (NORMSEQ);				/* turn on */
    }

/*
 *	grab some variables from the environment
 */
    if ((p = getenv ("SHELL")) != 0)
	hisshell = p;					/* his shell */
    if ((p = getenv ("NFED")) != 0)
	hised = p;					/* and his editor */
    else
	if ((p = getenv ("EDITOR")) != 0)		/* try this name */
	    hised = p;					/* his editor */

/*
 *	Scan the command line now to pluck options and
 *	notesfile specifications
 */

    if (*calledas == '=' && calledas[1] != '\0' && argc == 1)/* a =xxx case */
    {
	expand (&calledas[1]);				/* nf name */
	notesargs++;
    }

    for (i = 1; i < argc; i++)
    {
	if (argv[i][0] == '-')				/* option */
	{
	    switch (argv[i][1])
	    {
		case 's': 				/* sequencer */
		case 'x': 				/* extended sequencer */
		case 'i': 				/* index sequencer */
		case 'n': 				/* no sequencer */
		    expand (argv[i]);			/* change mode */
		    break;

		case 't': 				/* term type */
		    if (++i == argc)
		    {
			fprintf (stderr, "-t must be followed by terminal type\n");
			exit (BAD);
		    }
		    histty = argv[i];
		    break;

		case 'f': 				/* -f file option */
		    if (++i == argc)			/* see if there is one */
		    {
			fprintf (stderr, "-f must be followed by a file name\n");
			exit (BAD);
		    }
		    readrc (argv[i]);			/* load the file */
		    notesargs++;			/* supplied args */
		    break;

		case 'a': 				/* alternate sequencer */
		    if (++i == argc)			/* supplied name? */
		    {
			fprintf (stderr, "-a requires a name\n");
			exit (BAD);
		    }
		    else
		    {
			struct auth_f   whoami;
			getname (&whoami, 0);		/* grab real name */
			sprintf (Seqname, "%s:%s", whoami.aname, argv[i]);
		    }
		    break;

		case 'o': 				/* zero date */
		    if (++i == argc)			/* give a date? */
		    {
			fprintf (stderr, "-o requires a date\n");
			exit (BAD);
		    }
		    else
		    {
			struct when_f   ztime;

			switch (parsetime (argv[i], &ztime))
			{
			    case 0: 			/* ok */
				Basetime = ztime;	/* store it */
				setseq (USERSEQ);	/* readonly */
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
		    }
		    break;

		default: 
		    fprintf (stderr, "Bad switch: `%c'\n", argv[i][1]);
	    }
	}
	else						/* is a notesfile */
	{
	    expand (argv[i]);				/* add it to list */
	    notesargs++;				/* supplied args */
	}
    }

/*
 *	now, check some things and let it fly
 */

    if (autoseq)					/* grab sequencer */
    {
	if ((q = getenv ("NFSEQ")) != NULL)		/* grab NFSEQ */
	{
	    expand (q);
	}
	else
	{
	    char    rcbuf[WDLEN];
	    sprintf (rcbuf, "%s/%s/%s", Mstdir, UTILITY, SEQFILE);
	    if (readrc (rcbuf) != 0)			/* system list */
		expand (DFLTSEQ);			/* give him default */
	}
    }

    if (!autoseq && !notesargs)				/* wants a list */
    {
	usage ();
    }

    intflag = 0;					/* none yet */
    catchem ();						/* catch interupts */
    ttystrt ();						/* CBREAK mode */

    nfsdone = 0;					/* count groups */
    while ((nfptr = nextgroup ()) != (struct nflist_f *) NULL)
    {
	switch (control (nfptr -> nf_name, (int) nfptr -> nf_seqmode))
	{
	    case QUITFAST: 				/* leave now */
	    case QUITUPD: 				/* likewise */
		goto leaving;

	    case QUITBAD: 				/* err in name or nf */
	    case QUITNEX: 				/* no nf there */
	    case (-1): 					/* "normal" return */
	    default: 
		break;
	}
	nfsdone++;					/* count the ones done */
    }
    if (nfsdone == 0)					/* check if did any */
    {
	ttystop ();					/* turn it off */
	printf ("No notesfiles processed\n");
	fflush (stdout);
	exit (GOOD);
    }

leaving: 						/* get out of here */
    at (0, 1);						/* at bottom LHS */
    putc ('\n', stdout);
    ttystop ();						/* back to normal */
    exit (GOOD);
}

/*
 *	tell him how to invoke the program
 */

usage ()
{
    char    cmdline[CMDLEN];				/* build a command */
    char   *command;
    fprintf (stderr,
	    "Usage: %s [-s] [-t ttytype] [-f file] [-a seqname] topic [...]\n",
	    Invokedas);
    fprintf (stderr, "Hit <return> to continue\n");	/* ponder this */
    getc (stderr);					/* thanks to harpo!ber 4/30/82 */
    if ((command = getenv ("PAGER")) == NULL)		/* overridden? */
	command = PAGER;				/* assign default */
#ifndef	FASTFORK
    sprintf (cmdline, "%s < %s/%s/%s", command, Mstdir, UTILITY, AVAILHLP);
    dounix (cmdline, 1, 0);				/* print the list */
#else
    sprintf (cmdline, "%s/%s/%s", Mstdir, UTILITY, AVAILHLP);
    dounix (1, 0, command, cmdline, 0, 0, 0);
#endif
    exit (BAD);
}
