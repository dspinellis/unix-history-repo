static char *sccsid = "@(#)nfpipe.c	1.1\t1/23/83";

#include "parms.h"
#include "structs.h"
#include "globs.h"
/*
 *	nfpipe - allow a user to put this at the end of a pipe
 *	to catch everything that comes at it. The program 
 *	merely checks arguement 1 for the name of the notefile,
 *	verifys write permission (exiting if none) and then
 *	places the note into the notefile.
 *	the second arguement is the title to be of the note.
 *	THis can be null in which case the mult routine will insert
 *	a title consisting of the first TITLEN characters or
 *	or so of the note text.
 *
 *	Original coding:	Ray Essick 	January 1982
 */

main (argc, argv)
char  **argv;
{
    struct io_f io;
    struct note_f   note;
    struct daddr_f  where;
    struct auth_f   auth;
    struct id_f unique;
    char   *nfname,
           *ntitle;
    int     stat,
            anon,
            i;
    char    title[TITLEN];				/* title */

#include "main.i"					/* common init code and such */

    if (argc == 1) {				/* not even a notefile - bad */
	printf ("Usage: %s notefile [-t title] [-d] [-a]\n", argv[0]);
	exit (BAD);
    }
    stat = anon = 0;				/* set up flags and such */
    ntitle = NULL;				/* empty title */
    unique.uniqid = 0;				/* make mult do assignment */
    for (i = 1; i < argc; i++)
	if (argv[i][0] != '-')
	    nfname = argv[i];			/* point notefile name */
	else
	    switch (argv[i][1])
	    {
		case 'd': 
		    stat = DIRMES;		/* enable director message */
		    break;
		case 't': 			/* this one is the title */
		    ntitle = argv[++i];		/* grab title */
		    break;
		case 'a': 
		    anon = 1;
		    break;
		default: 
		    printf ("Bad switch: %c\n", argv[i][1]);
		    exit (BAD);
	    }
    if (init (&io, nfname) < 0) {
	exit (NONF);			/* no notefile - leave quickly */
    }

    if ((io.access & WRITOK) == 0) {
	printf ("You haven't write permission\n ");
	finish (&io);
	exit (BAD);
    }

    if (globuid == ANONUID) {
	printf ("You have the wrong uid for notefiles, talk to\n");
	printf ("the notesfiles manager\n");
	finish (&io);
	exit (BAD);
    }

    if (allow (&io, DRCTOK) == 0) {
	stat &= NOT DIRMES;
    }

    if ((io.descr.d_stat & ANONOK) == 0) {
	anon = 0;
    }
    getname (&auth, anon);				/* grab name */
    gettime (&note.n_date);
    if (ntitle == NULL)	 {				/* no title specified */
	ntitle = "Through nfpipe";
    }

    for (i = 0; i < TITLEN && *ntitle; i++) {
	title[i] = *ntitle++;
    }

    for (; i < TITLEN; i++) {
	title[i] = ' ';
    }

    pagein (&io, stdin, &where);			/* put it in there */
    putnote (&io, &where, title, stat, &note, &auth, NOPOLICY, LOCKIT, COPYID, SYSTEM, 1);
    finish (&io);					/* close shop */
    exit (GOOD);
}
