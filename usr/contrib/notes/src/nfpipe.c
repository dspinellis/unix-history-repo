#define	MAINLINE
#include "parms.h"
#include "structs.h"

#ifdef	RCSIDENT
static char rcsid[] = "$Header: nfpipe.c,v 1.7.0.1 85/10/06 01:41:35 notes Rel $";
#endif	RCSIDENT

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

    startup (argc, argv);				/* common initialization */

    if (argc == 1)					/* not even a notefile - bad */
    {

	printf ("Usage: %s notefile [-t title] [-d] [-a]\n", argv[0]);

	exit (BAD);
    }
    stat = anon = 0;					/* set up flags and such */
    ntitle = NULL;					/* empty title */
    unique.uniqid = 0;					/* make mult do assignment */
    for (i = 1; i < argc; i++)
	if (argv[i][0] != '-')
	    nfname = argv[i];				/* point notefile name */
	else
	    switch (argv[i][1])
	    {
		case 'd': 
		    stat = DIRMES;			/* enable director message */
		    break;

		case 't': 				/* this one is the title */
		    ntitle = argv[++i];			/* grab title */
		    break;


		case 'a': 
		    anon = 1;
		    break;

		default: 
		    printf ("Bad switch: %c\n", argv[i][1]);
		    exit (BAD);
	    }

    if (init (&io, nfname) < 0)
	exit (NONF);					/* no notefile - leave quickly */

    if (allow (&io, WRITOK) == 0)
    {
	printf ("You haven't write permission\n ");
	finish (&io);
	exit (BAD);
    }

    if (globuid == Anonuid)
    {
	printf ("You have the wrong uid (%d) for notefiles, talk to the system manager\n",
		globuid);
	finish (&io);
	exit (BAD);
    }

#ifndef	WRITEARCH					/* allowing writes */
    if ((io.descr.d_stat & ISARCH) && !allow (&io, DRCTOK))
    {
	at (0, 10);
	printf ("Sorry, you can not write in an archive");
	fflush (stdout);
	exit (BAD);
    }
#endif	WRITEARCH

    if (allow (&io, DRCTOK) == 0)
	stat &= NOT DIRMES;
    if ((io.descr.d_stat & ANONOK) == 0)
	anon = 0;

    getname (&auth, anon);				/* grab name */

    gettime (&note.n_date);
    if (ntitle == NULL)					/* no title given */
	ntitle = "Through nfpipe";
    else
	strclean (ntitle);				/* zap control chars */

    for (i = 0; i < TITLEN; i++)
	if ((title[i] = *ntitle++) == '\0')		/* want assignment */
	    break;
    title[TITLEN - 1] = '\0';				/* sure it stops */
    pagein (&io, stdin, &where);			/* put it in there */
    putnote (&io, &where, title, stat, &note, &auth, NOPOLICY, LOCKIT, COPYID, System, 1);
    finish (&io);					/* close shop */
    exit (GOOD);
}
