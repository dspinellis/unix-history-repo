static char *sccsid = "@(#)nfarchive.c	1.1\t1/23/83";

#include "parms.h"
#include "structs.h"
#include "globs.h"
/*
 *	nfarchive - will archive the specified notesfiles. All notes that
 *	have not been modofied in the last n days (default 14, settable
 *	by option on command line) will be archived and placed in
 *	the archive directory.
 *
 *	Ray Essick			March 1982
 */

int     deleteonly;
int     dirmsgflag;
int     daysold;

archone (nfname)
char   *nfname;
{
    return(archiver(nfname, daysold, deleteonly, dirmsgflag));
}


main (argc, argv)
char  **argv;
{
    char    nf[NNLEN];
    FILE * altfile;
    int     i,
            count,
            c;

#include "main.i"			/* common init code and such */

    if (argc == 1) {
	printf ("Usage: %s [-d] [-m+ or -m-] [-nn] [-f file] topic1 [topic2 ...]\n", argv[0]);
	exit (BAD);
    }

    if (globuid != NOTESUID) {
	printf ("Sorry, only notes 'owner' can archive notes\n");
	exit (BAD);
    }

    deleteonly = 0;			/* default to archiving */
    dirmsgflag = DIRNOCARE;		/* don't consider dirmessage */
    daysold = ARCHTIME;			/* default to 2 weeks */

    for (i = 1; i < argc; i++) {
	if (argv[i][0] == '-') {
	    switch (argv[i][1]) {
		case 'd': 		/* delete only, no archiving */
		    deleteonly = 1;
		    break;

		case 'm': 		/* director message status */
		    if (argv[i][2] == '+')
			dirmsgflag = DIRON;	/* delete only with on */
		    else
			dirmsgflag = DIROFF;	/* only if off */
		    break;

		case '1': 		/* change days old parameter */
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

		case 'f': 			/* process a file */
		    readrc(argv[++i]);
		    break;

		default: 
		    printf ("Bad switch: %c\n", argv[i][1]);
		    exit (BAD);
	    }
	} else {
	    expand(argv[i]);
	}
    }
    for (i = 0; i < last_group; i++) {
	if (group[i].lookat == 1) {
	    archone(group[i].name);
	}
    }
}
