static char *sccsid = "@(#)checknotes.c	1.1\t1/23/83";

#include "parms.h"
#include "structs.h"
#include "globs.h"
/*
 *	checknotes
 *
 *	A program which prints the notesfiles modified since the
 *	user's last entry. The output is a list of the modified
 *	notesfiles. 
 *	
 *	The program exits with a zero status if there ARE modified
 *	notesfiles, and non-zero if there are not. This is to
 *	correspond with the "true" and "false" programs and 
 *	to permit use of the program in shell scripts.
 *
 *	The optional "-q" parameter disables the printing of the
 *	names of the notesfiles, the exit code is the only metho
 *	of determining the existence of new notes.
 *
 *	Original Coding:	Ray Essick	June 12, 1982
 */

#define	VERBQ	0
#define	VERBN	1
#define	VERBV	2
#define	VERBS	3

int     verbosity = VERBQ;				/* be verbose */
int     ncount = 0;			/* number of modified notesfiles */
char    *strsave();

checknf (nfname)
char   *nfname;
{
    struct io_f io;
    struct auth_f   whoami;
    int     retcode;

    if (opennf (&io, nfname) < 0) {
	return(0);					/* ignore this one */
    }
    getdscr(&io, &io.descr);
    getname (&whoami, 0);				/* grab my name */
    getlast (&io.stime, nfname, NORMSEQ, whoami.aname); /* last date */
    if (inorder (&io.stime, &io.descr.d_lastm)) {
	ncount++;			/* bump count of touched notesfiles */
	if (verbosity == VERBV) {
	    printf ("%s\n", nfname);
	}
	if (verbosity == VERBQ || verbosity == VERBS) {
	    retcode = QUITFAST;
	} else {
	    retcode = 1;
	}
    } else {
	retcode = 0;
    }
    closenf (&io);				/* close the notesfile */
    return(retcode);
}


main (argc, argv)
char  **argv;
{
    char   *p;
    int     i;					/* hold characters */
    char    cmdline[CMDLEN];			/* buffer names */
    int notesargs = 0;

#include "main.i"				/* do common initializations */

    for (i = 1; i < argc; i++)
	if (argv[i][0] == '-') {			/* see if options */
	    switch (argv[i][1]) {
		case 'q': 				/* "are new notes" */
		    verbosity = VERBQ;
		    break;

		case 'n': 			/* spit "no new notes" */
		    verbosity = VERBN;
		    break;

		case 'v': 				/* tell everything */
		    verbosity = VERBV;
		    break;

		case 's': 				/* silent */
		    verbosity = VERBS;
		    break;

		case 'f':
		    notesrc = argv[++i];
		    break;

		default: 
		    fprintf (stderr, "Usage: %s [-qvns] [-f file]\n", argv[0]);
		    exit(BAD);
	    }

	} else {
	    expand(argv[i]);
	    notesargs++;
	}

    if (notesrc) {
	if (readrc(notesrc) < 0) {
		perror(notesrc);
		exit(BAD);
	}
    } else if (notesargs == 0) {
	p = getenv("HOME");
	if (!p) p = ".";
	sprintf(cmdline, "%s/%s", p, NOTESRC);
	readrc(cmdline);		/* ignore return status */
    }
    for (i = 0; i < last_group; i++) {
	if (group[i].lookat == 1) {
	    if(checknf(group[i].name)==QUITFAST)
		break;
	}
    }
    switch (verbosity) {
	case VERBQ: 					/* "there are new" */
	    if (ncount) {
		printf ("There are new notes\n");
	    }
	    break;

	case VERBN: 					/* there are no new */
	    if (ncount == 0) {
		printf ("There are no new notes\n");
	    }
	    break;

	case VERBV: 					/* told elsewhere */
	case VERBS: 					/* silent */
	default: 
	    break;
    }
    if (ncount) {
	exit(GOOD);			/* TRUE/success for the shell */
    } else {
	exit(BAD);			/* FALSE/failure to the shell */
    }
}
