#define	MAINLINE
#include "parms.h"
#include "structs.h"

#ifdef	FASTSEQ
#include	<sys/types.h>
#include	<sys/stat.h>
#endif	FASTSEQ

#ifdef	RCSIDENT
static char rcsid[] = "$Header: checknotes.c,v 1.7.0.1 85/02/01 09:56:54 notes Rel $";
#endif	RCSIDENT

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
int     ncount = 0;					/* number of modified notesfiles */
struct nflist_f *nfptr;

checknf (nfname)
char   *nfname;
{
    struct io_f io;
    int     retcode;


#ifdef	FASTSEQ
    {							/* for FAST checknotes ... */
	struct when_f   whenvec;
	char    NoteFile[WDLEN];
	struct stat StatBuf;

	if (nfname[0] == '/')				/* absolute pathname */
	{
	    getlast (&io.stime, rindex (nfname, '/') + 1, NORMSEQ, Seqname);
	    sprintf (NoteFile, "%s/%s", nfname, TEXT);
	}
	else						/* relative to Mstdir */
	{
	    getlast (&io.stime, nfname, NORMSEQ, Seqname);
	    sprintf (NoteFile, "%s/%s/%s", Mstdir, nfname, TEXT);
	}
	if (stat (NoteFile, &StatBuf) >= 0)
	{
	    maketime (&whenvec, (long) StatBuf.st_mtime);
	    if (inorder (&whenvec, &io.stime))
	    {
		return (0);
	    }
	    /* 
	     * not in order means that we should check the hard way
	     */
	}
    }
#endif	FASTSEQ

/*
 *	Do it the hard way -- open the notesfile and read the descriptor
 */
    if (opennf (&io, nfname) < 0)
	return 0;					/* ignore this one */
    getdscr (&io, &io.descr);
    getlast (&io.stime, io.nf, NORMSEQ, Seqname);	/* last date */
    if (inorder (&io.stime, &io.descr.d_lastm))
    {
	ncount++;					/* bump count of touched notesfiles */
	if (verbosity == VERBV)
	    printf ("%s\n", nfname);
	if (verbosity == VERBQ || verbosity == VERBS)
	    retcode = QUITFAST;
	else
	    retcode = 1;
    }
    else
	retcode = 0;
    closenf (&io);					/* close the notesfile */
    return retcode;
}


main (argc, argv)
char  **argv;
{
    char   *p;
    int     i;

    startup (argc, argv);				/* common initialization */

    if ((p = getenv ("NFSEQ")) == NULL)
	p = DFLTSEQ;					/* use default */

    expand (p);						/* put it in */

    for (i = 1; i < argc; i++)
	if (argv[i][0] == '-')				/* see if options */
	    switch (argv[i][1])
	    {
		case 'q': 				/* "are new notes" */
		    verbosity = VERBQ;
		    break;

		case 'n': 				/* spit "no new notes" */
		    verbosity = VERBN;
		    break;

		case 'v': 				/* tell everything */
		    verbosity = VERBV;
		    break;

		case 's': 				/* silent */
		    verbosity = VERBS;
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

		default: 
		    fprintf (stderr, "Usage: %s [-qvns] [-a seqname]\n", argv[0]);
		    exit (BAD);
	    }
	else						/* a notesfile! */
	    expand (argv[i]);				/* put it in */

    while ((nfptr = nextgroup ()) != (struct nflist_f *) NULL)
    {
	i = checknf (nfptr -> nf_name);
	if (i == QUITFAST)
	    break;					/* don't need more */
    }

    switch (verbosity)
    {
	case VERBQ: 					/* "there are new" */
	    if (ncount)
		printf ("There are new notes\n");
	    break;

	case VERBN: 					/* there are no new */
	    if (ncount == 0)
		printf ("There are no new notes\n");
	    break;

	case VERBV: 					/* told elsewhere */
	case VERBS: 					/* silent */
	default: 
	    break;
    }
    if (ncount)
	exit (0);					/* TRUE/success for the shell */
    else
	exit (1);					/* FALSE/failure to the shell */

}
