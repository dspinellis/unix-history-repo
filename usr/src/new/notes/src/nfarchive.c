#define	MAINLINE
#include "parms.h"
#include "structs.h"

#ifdef	RCSIDENT
static char rcsid[] = "$Header: nfarchive.c,v 1.7 85/01/18 15:25:22 notes Rel $";
#endif	RCSIDENT

/*
 *	nfarchive - will archive the specified notesfiles. All notes that
 *	have not been modofied in the last n days (default 14, settable
 *	by option on command line) will be archived and placed in
 *	the archive directory.
 *
 *	Ray Essick			March 1982
 */

int     deleteonly;
int     dirmsgflag;					/* key on dir msg */
int     daysold = ARCHTIME;				/* eligibility age */
int     worksetsize;					/* minimum to keep */

archone (nfname)
char   *nfname;
{
    return archiver (nfname, daysold, worksetsize, deleteonly, dirmsgflag);
}


main (argc, argv)
char  **argv;
{
    struct nflist_f *nfptr;
    int     i;

    startup (argc, argv);				/* common init */

    if (argc == 1)
    {
	printf ("Usage: %s [-d] [-m+ or -m-] [-#] [-w#] [-f file] topic1 [topic2 ...]\n", argv[0]);
	exit (BAD);
    }

    if (globuid != Notesuid)
    {
	printf ("Sorry, only notes 'owner' can archive notes\n");
	exit (BAD);
    }

    deleteonly = 0;					/* default to archiving */
    dirmsgflag = DIRNOCARE;				/* don't consider dirmessage */
    daysold = ARCHTIME;					/* default threshold */
    worksetsize = WORKSETSIZE;				/* default to leave */

    for (i = 1; i < argc; i++)
    {
	if (argv[i][0] == '-')
	    switch (argv[i][1])
	    {
		case 'd': 				/* delete only, no archiving */
		    deleteonly = 1;
		    break;

		case 'm': 				/* director message status */
		    if (argv[i][2] == '-')
			dirmsgflag = DIROFF;		/* delete when off */
		    else
			dirmsgflag = DIRON;		/* only when on */
							/* also if just "-m" */
		    break;

		case 'w': 				/* change workset size */
		    if (argv[i][2] == '\0')
			worksetsize = WORKSETSIZE;	/* default */
		    else
			worksetsize = atoi (&argv[i][2]);/* specified */
		    break;

		case '0': 				/* allow 0 days */
		case '1': 				/* change days old parameter */
		case '2': 
		case '3': 
		case '4': 
		case '5': 
		case '6': 
		case '7': 
		case '8': 
		case '9': 
		    daysold = atoi (&argv[i][1]);
		    break;

		case 'f': 				/* process a file */
		    if (++i == argc)			/* no filename */
		    {
			fprintf (stderr, "-f must be followed by filename\n");
			exit (BAD);
		    }
		    readrc (argv[i]);
		    break;

		default: 
		    printf ("Bad switch: %c\n", argv[i][1]);
		    exit (BAD);
	    }
	else
	    expand (argv[i]);				/* load it */
    }

/*
 *	now process things
 */

    while ((nfptr = nextgroup ()) != (struct nflist_f *) NULL)
	archone (nfptr -> nf_name);

    exit (GOOD);
}
